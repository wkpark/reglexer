<?php
/**
 * Author Markus Baker: http://www.lastcraft.com
 * Version adapted from Simple Test: http://sourceforge.net/projects/simpletest/
 * For an intro to the Lexer see:
 * http://www.phppatterns.com/index.php/article/articleview/106/1/2/
 *
 * @author Won-Kyu Park <wkpark@gmail.com>
 * @author Marcus Baker
 * @package Lexer
 * @license LGPLv2.1
 * @version    $Id: parser.php 1723 2008-04-08 00:34:10Z lastcraft $
 */

/**#@+
 * Lexer mode stack constants
 */
foreach (array('LEXER_ENTER', 'LEXER_MATCHED',
                'LEXER_UNMATCHED', 'LEXER_EXIT',
                'LEXER_SPECIAL') as $i => $constant) {
    if (! defined($constant)) {
        define($constant, $i + 1);
    }
}
/**#@-*/

/**
 *    Compounded regular expression. Any of
 *    the contained patterns could match and
 *    when one does, it's label is returned.
 *    @package Lexer
 */
class LexerParallelRegex
{
    var $_patterns;
    var $_labels;
    var $_regex;
    var $_case;
    var $_modifier;
    var $_patterns_index;

    /**
     *    Constructor. Starts with no patterns.
     *    @param boolean $case    True for case sensitive, false
     *                            for insensitive.
     *    @param string $modifier regex pattern modifiers.
     *    @access public
     */
    function LexerParallelRegex($case = true, $modifier = 'msS')
    {
        $this->_case = $case;
        $this->_modifier = $modifier;
        $this->_patterns = array();
        $this->_labels = array();
        $this->_regex = null;
        $this->_patterns_index = array();
    }

    /**
     *    Adds a pattern with an optional label.
     *    @param string $pattern      Perl style regex, but ( and )
     *                                lose the usual meaning.
     *    @param string $label        Label of regex to be returned
     *                                on a match.
     *    @access public
     */
    function addPattern($pattern, $label = true)
    {
        $count = count($this->_patterns);
        $this->_patterns[$count] = $pattern;
        $this->_labels[$count] = $label;
        $this->_regex = null;
    }

    /**
     *    Attempts to match all patterns at once against
     *    a string.
     *    @param string $subject      String to match against.
     *    @param string[] $match      Matched array of subject.
     *    @param integer $pos         Matched position.
     *    @param integer $flags       optional match flags like as PREG_OFFSET_CAPTURE.
     *    @return boolean             True on success.
     *    @access public
     */
    function match($subject, &$matches, &$pos = 0, $flags = 0)
    {
        if (!isset($this->_patterns[0])) {
            $matches = array();
            return false;
        }

        if (! preg_match($this->_getCompoundedRegex(), $subject, $matches, PREG_OFFSET_CAPTURE, $pos)) {
            $matches = array();
            return false;
        }

        $idx = 1;
        foreach ($this->_patterns_index as $k=>$v) {
            if (isset($matches[$k][1]) && $matches[$k][1] != -1) {
                // the matched index
                $idx = $v;

                // get the matched offset.
                $offset = $matches[$k][1];

                // remove not matched portion
                array_splice($matches, 0, $k);
                if (($flags & PREG_OFFSET_CAPTURE) != PREG_OFFSET_CAPTURE) {
                    // matched column only same as array_column($matches, 0)
                    $matched = array();
                    foreach ($matches as $i=>$m)
                        $matched[$i] = $m[0];
                    $matches = $matched;
                }
                // return offset
                $pos = $offset;

                return $this->_labels[$idx];
            }
        }

        // no label found.
        return true;
    }

    /**
     *    Attempts to split the string against all patterns at once
     *
     *    @param string $subject      String to match against.
     *    @param array $split         The split result: array containing, pre-match, matches & post-match strings
     *    @return boolean             True on success.
     *    @access public
     *
     *    @author Christopher Smith <chris@jalakai.co.uk>
     */
    function split($subject, &$split)
    {
        if (!isset($this->_patterns[0])) {
            return false;
        }

        if (! preg_match($this->_getCompoundedRegex(), $subject, $matches)) {
            $split = array($subject, array(), "");
            return false;
        }

        $idx = 1;
        foreach ($this->_patterns_index as $k=>$v) {
            if (!empty($matches[$k]) && isset($this->_labels[$v])) {
                $idx = $v;
                if (isset($this->_labels[$v])) {
                    // remove not matched portion
                    array_splice($matches, 0, $k);
                }
                break;
            }
        }

        list($pre, $post) = preg_split('/'.$this->_patterns[$idx].'/'.$this->_getPerlMatchingFlags(), $subject, 2);

        $split = array($pre, $matches, $post);
        return isset($this->_labels[$idx]) ? $this->_labels[$idx] : true;
    }

    /**
     *    Compounds the patterns into a single
     *    regular expression separated with the
     *    "or" operator. Caches the regex.
     *    Will automatically escape (, ) and / tokens.
     *    @param array $patterns    List of patterns in order.
     *    @access public
     */
    function getRawCompoundedRegex()
    {
        if ($this->_regex == null) {
            $patterns = array();
            $patterns_index = array();
            $offset = 0;
            for ($j = 0, $count = count($this->_patterns); $j < $count; $j++) {
                // check already wraped by ()
                if (preg_match('/^\((?!\?).*\)$/s', $this->_patterns[$j])) {
                    $nowrap = true;
                } else {
                    $nowrap = false;
                }

                /*
                 * decompose the input pattern into "(", "(?", ")",
                 * "[...]", "[]..]", "[^]..]", "[...[:...:]..]", "\", "\x"...
                 * elements.
                 */
                preg_match_all('/\\\\[^0-9]|' .
                               '\\\\|' .
                               '\(\?|' .
                               '[()]|' .
                               '\d+|' .
                               '\[\^?\]?(?:\\\\.|\[:[^]]*:\]|[^]\\\\])*\]|' .
                               '[^[()\\\\]+/s', $this->_patterns[$j], $matches);

                $chunks = $matches[0];

                $level = 0;
                $captures = 0;
                $referenced = array();
                for ($i = 0, $cnt = count($chunks); $i < $cnt; $i++) {
                    $chunk = $chunks[$i];

                    // recheck nowrap
                    if ($i > 0 && $nowrap && $level == 0)
                        $nowrap = false;

                    // count capture pattern and fix back-reference offsets
                    switch($chunk) {
                        case '(': // (?(1)..) style back-reference
                            $level++;
                        case '\\': // \\1 style back-reference
                            if (isset($chunks[$i + 1])) {
                                if (!is_numeric($chunks[$i + 1])) {
                                    if ($chunk == '(')
                                        $captures++;
                                    continue;
                                }
                                if ($chunks[$i] == '\\' || $chunks[$i - 1] == '(?')
                                    $referenced[$i + 1] = $chunks[$i + 1];
                            }
                            break;
                        case ')':
                            $level--;
                            break;
                        case '(?':
                            $level++;
                            if (isset($chunks[$i + 2]) && is_numeric($chunks[$i + 1]) && $chunks[$i + 2] == ')') {
                                // (?1) style recursive reference
                                $referenced[$i + 1] = $chunks[$i + 1];
                                $i+= 2;
                                $level--;
                            }
                            break;
                        default:
                            // \1 \2 referenced cases
                            if (ctype_cntrl($chunk)) {
                                $referenced[$i] = $chunk;
                                break;
                            }
                            //if (substr($chunk, 0, 1) != '\\')
                            //    $chunks[$i] = str_replace('/', '\\/', $chunk);
                    }
                }

                $wrap = 0;
                if (!$nowrap)
                    $wrap = 1; // a pattern will be wrapped and back-reference offset is changed off by one.

                foreach ($referenced as $k=>$reference) {
                    if (ctype_cntrl($reference)) {
                        $ref = ord($reference);
                        // Is it a valid back reference ?
                        if ($ref <= $captures) {
                            $oref = $ref;
                            $ref += $wrap + $offset;
                            $chunks[$k] = "\\".$ref;
                            continue;
                        }
                    }
                    if ($reference < 0)
                        $reference += $captures + $wrap + $offset + 1;
                    else
                        $reference += $wrap + $offset;
                    $chunks[$k] = $reference; // fix reference number
                }
                $pattern = implode('', $chunks);
                $patterns_index[$offset + 1] = $j; // j-th pattern will be captured at $offset + 1

                if ($nowrap) {
                    $patterns[] = $pattern;
                } else {
                    $patterns[] = '('.$pattern.')'; // each pattern must be wrapped
                }

                if (!$nowrap)
                    $offset++;
                $offset += $captures;
            }
            $this->_patterns_index = $patterns_index;

            $this->_regex = implode('|', $patterns);
        }
        return $this->_regex;
    }

    /**
     *    Compounds the patterns into a single
     *    regular expression separated with the
     *    "or" operator.
     *    @param array $patterns    List of patterns in order.
     *    @access private
     */
    function _getCompoundedRegex()
    {
        return '/' . $this->getRawCompoundedRegex() . '/' . $this->_getPerlMatchingFlags();
    }

    /**
     *    Accessor for perl regex mode flags to use.
     *    @return string       Perl regex flags.
     *    @access private
     */
    function _getPerlMatchingFlags()
    {
        return ($this->_case ? $this->_modifier : $this->_modifier.'i');
    }
}

/**
 *    States for a stack machine.
 *    @package Lexer
 */
class LexerStateStack
{
    var $_stack;
    var $_count;

    /**
     *    Constructor. Starts in named state.
     *    @param string $start        Starting state name.
     *    @access public
     */
    function LexerStateStack($start)
    {
        $this->_stack = array($start);
        $this->_count = 1;
    }

    /**
     *    Accessor for current state.
     *    @return string       State.
     *    @access public
     */
    function getCurrent()
    {
        return $this->_stack[$this->_count - 1];
    }

    /**
     *    Adds a state to the stack and sets it
     *    to be the current state.
     *    @param string $state        New state.
     *    @access public
     */
    function enter($state)
    {
        array_push($this->_stack, $state);
        $this->_count ++;
    }

    /**
     *    Leaves the current state and reverts
     *    to the previous one.
     *    @return boolean    False if we drop off
     *                       the bottom of the list.
     *    @access public
     */
    function leave()
    {
        if ($this->_count == 1) {
            return false;
        }
        array_pop($this->_stack);
        $this->_count --;
        return true;
    }
}

/**
 *    Accepts text and breaks it into tokens.
 *    Some optimisation to make the sure the
 *    content is only scanned by the PHP regex
 *    parser once. Lexer modes must not start
 *    with leading underscores.
 *    @package RegLexer
 */
class RegLexer
{
    var $_regexes;
    var $_parser;
    var $_mode;
    var $_mode_start;
    var $_mode_handlers;
    var $_case;
    var $_modifier;

    /**
     *    Sets up the lexer in case insensitive matching
     *    by default.
     *    @param Parser $parser           Handling strategy by
     *                                    reference.
     *    @param string $start            Starting handler.
     *    @param boolean $case            True for case sensitive.
     *    @param string $modifier         regex pattern modifiers.
     *    @access public
     */
    function RegLexer(&$parser, $start = "accept", $case = true, $modifier = 'msS')
    {
        $this->_case = $case;
        $this->_modifier = $modifier;
        $this->_regexes = array();
        $this->_parser = &$parser;
        $this->_mode_start = $start;
        $this->_mode = new LexerStateStack($start);
        $this->_mode_handlers = array($start => $start);
    }

    /**
     *    Adds a token search pattern for a particular
     *    parsing mode. The pattern does not change the
     *    current mode.
     *    @param string $pattern      Perl style regex, but ( and )
     *                                lose the usual meaning.
     *    @param string $mode         Should only apply this
     *                                pattern when dealing with
     *                                this type of input.
     *    @param string $label        additional label for lowlevel operation
     *                                can be another modename or others.
     *    @access public
     */
    function addPattern($pattern, $mode = "accept", $label = true)
    {
        if (! isset($this->_regexes[$mode])) {
            $this->_regexes[$mode] = new LexerParallelRegex($this->_case, $this->_modifier);
        }

        $this->_regexes[$mode]->addPattern($pattern, $label);
        if (! isset($this->_mode_handlers[$mode])) {
            $this->_mode_handlers[$mode] = $mode;
        }
    }

    /**
     *    Adds a pattern that will enter a new parsing
     *    mode. Useful for entering parenthesis, strings,
     *    tags, etc.
     *    @param string $pattern      Perl style regex, but ( and )
     *                                lose the usual meaning.
     *    @param string $mode         Should only apply this
     *                                pattern when dealing with
     *                                this type of input.
     *    @param string $new_mode     Change parsing to this new
     *                                nested mode.
     *    @access public
     */
    function addEntryPattern($pattern, $mode, $new_mode)
    {
        if (! isset($this->_regexes[$mode])) {
            $this->_regexes[$mode] = new LexerParallelRegex($this->_case, $this->_modifier);
        }
        $this->_regexes[$mode]->addPattern($pattern, $new_mode);
        if (! isset($this->_mode_handlers[$new_mode])) {
            $this->_mode_handlers[$new_mode] = $new_mode;
        }
    }

    /**
     *    Adds a pattern that will exit the current mode
     *    and re-enter the previous one.
     *    @param string $pattern      Perl style regex, but ( and )
     *                                lose the usual meaning.
     *    @param string $mode         Mode to leave.
     *    @access public
     */
    function addExitPattern($pattern, $mode)
    {
        if (! isset($this->_regexes[$mode])) {
            $this->_regexes[$mode] = new LexerParallelRegex($this->_case, $this->_modifier);
        }
        $this->_regexes[$mode]->addPattern($pattern, "__exit");
        if (! isset($this->_mode_handlers[$mode])) {
            $this->_mode_handlers[$mode] = $mode;
        }
    }

    /**
     *    Adds a pattern that has a special mode. Acts as an entry
     *    and exit pattern in one go, effectively calling a special
     *    parser handler for this token only.
     *    @param string $pattern      Perl style regex, but ( and )
     *                                lose the usual meaning.
     *    @param string $mode         Should only apply this
     *                                pattern when dealing with
     *                                this type of input.
     *    @param string $special      Use this mode for this one token.
     *    @access public
     */
    function addSpecialPattern($pattern, $mode, $special)
    {
        if (! isset($this->_regexes[$mode])) {
            $this->_regexes[$mode] = new LexerParallelRegex($this->_case, $this->_modifier);
        }
        $this->_regexes[$mode]->addPattern($pattern, "_$special");
        if (! isset($this->_mode_handlers[$special])) {
            $this->_mode_handlers[$special] = $special;
        }
    }

    /**
     *    Adds a mapping from a mode to another handler.
     *    @param string $mode        Mode to be remapped.
     *    @param string $handler     New target handler.
     *    @access public
     */
    function mapHandler($mode, $handler)
    {
        $this->_mode_handlers[$mode] = $handler;
    }

    /**
     *    Splits the page text into tokens. Will fail
     *    if the handlers report an error or if no
     *    content is consumed. If successful then each
     *    unparsed and parsed token invokes a call to the
     *    held listener.
     *    @param string $raw        Raw HTML text.
     *    @return boolean           True on success, else false.
     *    @access public
     */
    function parse($raw)
    {
        if (! isset($this->_parser)) {
            return false;
        }
        $length = strlen($raw);
        $rawpos = 0;

        $pos = 0;
        $newpos = 0;

        $rawoff = 0;
        $offset = 0;

        $lid = 1;
        while ($mode = $this->match($raw, $matches, $offset)) {
            $unmatched = substr($raw, $rawoff, $offset - $rawoff);

            $pos = $rawpos + $rawoff;
            $newpos = $rawpos + $offset;
            $consumed = $this->_dispatchTokens($unmatched, $matches, $mode, $pos, $newpos);
            if ($consumed === false) {
                return false;
            }

            if ($consumed === true || $consumed == 0) {
                $consumed = strlen($matches[0]);
                $offset += $consumed;
                $raw = substr($raw, $offset);
                if ($raw == '') {
                    break;
                }

                $length -= $offset;
                $rawpos += $offset;

                $rawoff = $offset = 0;
            } else {
                // shift offset
                $offset += $consumed;
                $rawoff = $offset;
            }
            $lid += substr_count($unmatched, "\n");
        }
        // check $rawoff and set remaining $raw
        if ($rawoff > 0) {
            $raw = substr($raw, $rawoff);
            $pos = $rawpos + $rawoff;
        }

        // close all tags.
        $consumed = $this->_invokeParser(array($raw), LEXER_UNMATCHED, $pos);
        $pos += $consumed;
        while ($this->_mode->getCurrent() != $this->_mode_start) {
            $this->_invokeParser(array(''), LEXER_EXIT, $pos);
        }

        return $consumed;
    }

    /**
     *    Sends the matched token or the matched result (by preg_match())
     *    and any leading unmatched text to the parser changing the lexer
     *    to a new mode if one is listed.
     *    @param string $unmatched    Unmatched leading portion.
     *    @param string/string[] $matches Actual matched text or matched array
     *    @param string $mode         Mode after match. A boolean
     *                                false mode causes no change.
     *    @param int $pos             Current byte index location in raw doc
     *                                thats being parsed
     *    @return boolean             False if there was any error
     *                                from the parser.
     *    @access private
     */
    function _dispatchTokens($unmatched, $matches, $mode = false, $initialPos, $matchPos)
    {
        if (isset($unmatched[0]) && ! $this->_invokeParser($unmatched, LEXER_UNMATCHED, $initialPos)) {
            return false;
        }
        if (is_bool($mode)) {
            return $this->_invokeParser($matches, LEXER_MATCHED, $matchPos);
        }
        if ($this->_isModeEnd($mode)) {
            return $this->_invokeParser($matches, LEXER_EXIT, $matchPos);
        }

        return $this->_invokeParser($matches, $mode, $matchPos);
    }

    /**
     *    Tests to see if the new mode is actually to leave
     *    the current mode and pop an item from the matching
     *    mode stack.
     *    @param string $mode    Mode to test.
     *    @return boolean        True if this is the exit mode.
     *    @access private
     */
    function _isModeEnd($mode)
    {
        return ($mode === "__exit");
    }

    /**
     *    Test to see if the mode is one where this mode
     *    is entered for this token only and automatically
     *    leaves immediately afterwoods.
     *    @param string $mode    Mode to test.
     *    @return boolean        True if this is the exit mode.
     *    @access private
     */
    function _isSpecialMode($mode)
    {
        return (strncmp($mode, "_", 1) == 0);
    }

    /**
     *    Strips the magic underscore marking single token
     *    modes.
     *    @param string $mode    Mode to decode.
     *    @return string         Underlying mode name.
     *    @access private
     */
    function _decodeSpecial($mode)
    {
        return substr($mode, 1);
    }

    /**
     *    Calls the parser method named after the current
     *    mode. Empty content will be ignored. The lexer
     *    has a parser handler for each mode in the lexer.
     *    @param string/string[] $matches  Matched or parsed text/array.
     *    @param string $mode         Token is recognised rather
     *                                than unparsed data.
     *    @param int $pos             Current byte index location in raw doc
     *                                thats being parsed
     *    @return int                 Consumed string length.
     *    @access private
     */
    function _invokeParser($matches, $mode, $pos, $raw = null)
    {
        if (empty($matches) && $mode !== LEXER_EXIT) {
            return true;
        }

        // numeric cases - normal LEXER_* mode
        // @label - low level LEXER_MATCHED cases
        if (is_numeric($mode) || $mode[0] == '@') {
            $parser_mode = $this->_mode->getCurrent();
        } else if ($this->_isSpecialMode($mode)) {
            $parser_mode = $this->_decodeSpecial($mode);
            $mode = LEXER_SPECIAL;
        } else {
            $parser_mode = $mode;
            $mode = LEXER_ENTER;
        }

        $handler = $this->_mode_handlers[$parser_mode];
        if (!is_array($matches))
            $matches = (array)$matches;
        $consumed = $this->_parser->$handler($matches, $mode, $pos, $raw);
        if ($consumed === false)
            return false;

        if ($mode === LEXER_EXIT)
            $this->_mode->leave();
        else if ($mode === LEXER_ENTER)
            $this->_mode->enter($parser_mode);
        return $consumed;
    }

    /**
     *    Tries to match a chunk of text and if successful
     *    removes the recognised chunk and any leading
     *    unparsed data. Empty strings will not be matched.
     *    @param string $raw         The subject to parse. This is the
     *                               content that will be eaten.
     *    @return array/boolean      Three item list of unparsed
     *                               content followed by the
     *                               recognised token and finally the
     *                               action the parser is to take.
     *                               True if no match, false if there
     *                               is a parsing error.
     *    @access public
     */
    function match($raw, &$matches, &$pos = 0)
    {
        if ($action = $this->_regexes[$this->_mode->getCurrent()]->match($raw, $matches, $pos)) {
            return $action;
        }
        return $action;
    }
}

// vim:et:sts=4:sw=4:
