'From Cuis 5.0 [latest update: #4249] on 1 July 2020 at 5:36:25 pm'!

!RunNotArray commentStamp: '<historical>' prior: 0!
A replacement for RunArray that does not optimize space. Essentially just an array, with a few idiosyncratic methods for compatibility with RunArray.

Rationale: When styling Smalltalk text, runs are very short. Space saving is not significant. Maybe 90% of the time is spent building and coalescing RunArrays. So, don't save space. Save time!!!


!Editor commentStamp: '<historical>' prior: 0!
New text editors.
TextEditor provides most of the functionality that used to be in TextMorphEditor.
SmalltalkEditor has Smalltalk code specific features.
SimpleEditor provides basic functionality for single line text editing. It does not handle fonts and styles, aligning and Smalltalk utilities. It handles one single line.!


!TextAlignment commentStamp: '<historical>' prior: 0!
Warning: TextAlignment and ParagraphStyleReference (What is this referring to?) should always be applied to whole 'paragraphs' in the text. See #isParagraphAttribute

(
(Text string: 'This text has no style set', String crString),
(Text string: 'This is centered', String crString attribute: TextAlignment centered),
(Text string: 'This text has no style set', String crString)
) edit!


!StringMorph commentStamp: '<historical>' prior: 0!
StringMorph is a "lightweight" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextModelMorph.

Structure:
instance var    	Type              Description 
font 			StrikeFont 		(normally nil; then the accessor #font gives back  a Font or nil #defaultFont) 
emphasis 		SmallInteger	bitmask determining character attributes (underline, bold, 	italics, struckThrough) 
contents 		String 			The text that will be displayed. 
!


!TaskbarMorph commentStamp: '<historical>' prior: 0!
A simple task bar written for Cuis.

dashBoard contains views/controls
viewBox contains graphic buttons of "iconized" windows/morphs.
scale allows 1x 2x 4x taskbar height. [scale= 1,2,4]!


!AutoCompleter commentStamp: '<historical>' prior: 0!
An InnerTextMorph can have an autocompleter in the same way it might have a styler. My instances implement autocompletion.!

