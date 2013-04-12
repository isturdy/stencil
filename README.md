Stencil
=======

A simple string template library with runtime template loading.

Note that runtime loading of templates prevents static checks of template (for invalid references, or invocation of a value of the wrong type. In such cases the library will produce the best output it can and a list of errors.


Template syntax
===============

Reserved characters and escaping
--------------------------------

In all contexts, Stencil reserves doubled and tripled angle brackets with an internal parenthesis ('<<(', ')>>') and pipes ('|'). If any of these are needed within the text, add one angle bracket or two pipes, as appropriate: '<<<(' becomes '<<(', '|||' '|', '|||| '||', and so on. For simplicity, these should be escaped everywhere, not merely where syntactically significant.

For disambiguation, variable names may not begin with '?', '%', '@', '!', '$', '&', and pipes and angle bracket-parenthesis sequences must be escaped.

Substitutions
-------------

    <<(name)>>

Substitute the value of *name* (which should be text or a Haskell value with a defined conversion to text) from the present context. In all substitutions, doubling the parentheses (`<<((name))>>`) results in the contained text being HTML escaped.

    <<(?name| text1 | text2 )>>

If *name* is in the dictionary, substitute *text1*; otherwise substitute *text2*.

    <<(%name| text )>>

Evaluate *text* in the context of the dictionary referred to by *name*. Within *text*, any lookups not found in that dictionary are passed to the next higher context.

    <<(@name| text | alternate text )>>

Substitute *text* for each item of the list referred to by *name*. If *name* is a list of texts, they can be accessed with '<<()>>' within *text*, so that `<ul><<(@listname| <li> <<()>> </li> )>></ul>` becomes `<ul> <li> item1 </li>  <li> item2 </li> </ul>`. If the list is empty, substitute the alternate text if present. if *name* refers to a list of dictionaries, each evaluation of *text* is in the context of the corresponding dictionary.

    <<(!functionname| valuename )>>

Substitute the result of applying the function to the value. Note that arbitrary Haskell is not allowed; the function used must be explicitly placed in the dictionary.

    <<($functionname| text )>>

Substitute the result of applying the function to the text, after making substitutions within the text.

    <<(&templatename)>>

Substitute and evaluate the named template.
