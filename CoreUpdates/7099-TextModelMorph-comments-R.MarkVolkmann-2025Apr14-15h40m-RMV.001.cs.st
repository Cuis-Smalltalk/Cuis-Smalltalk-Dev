'From Cuis7.3 [latest update: #7097] on 14 April 2025 at 3:43:38 pm'!

!TextModelMorph commentStamp: 'RMV 4/14/2025 15:43:26' prior: 0!
This is a morph that enables users to view and edit text.

The main features of this class include:
- using either a TextModel or TextProvider to hold the text being edited
- styling and text wrapping (provided by InnerTextMorph)
- scrolling with scrollbars (provided by superclass PluggableScrollPane)

There are two approaches for creating instances. One approach is to hold the text value in a TextModel object. The class methods withModel: and withText: support this. The other approach is to hold the text value in a TextProvider. The class methods that begin with textProvider: support this. For examples, see the method comments in those class methods.

The default background color is white. A border is only displayed when it has focus. One way to make its bounds apparent when it doesn't have focus is to set the background color of the parent component. Another way is to set the background color of this morph.

By default, words that would extend past the right side wrap to the next line. To prevent wrapping, send an instance #wrapFlag: with an argument of false.

The default size is 200 by 100, set in the initialize method of the superclass PluggableMorph. Depending on the font, the default size displays around four lines of wrapping text with around 17 characters per line. To change the size, send an instance #morphExtent: with a Point argument that specifies a width and height. The size should include space for scroll bars if they may be needed. Setting the height to zero causes it to actually be set to the height required for a single line in the current font.

Related classes include TextModel, TextProvider, PluggableScrollPane, and InnerTextMorph.!


!TextModelMorph class methodsFor: 'instance creation' stamp: 'RMV 4/14/2025 15:43:01'!
textProvider: aTextProvider textGetter: getTextSel textSetter: setTextSel
	"aTextProvider must have corresponding getter and setter methods
	that manage the text to be edited.
	For example, we can use an instance of the Exception class
	which has the methods messageText and messageText:.

	Select and evaluate the code below.
	In the Inspector that appears, select the messageText instance variable.
	In the TextModelMorph that appears, change the text
	and note how it updates in the Inspector.

	| exception morph |
	exception := Exception new.
	exception messageText: 'initial'.
	exception inspect.
	morph := TextModelMorph textProvider: exception textGetter: #messageText textSetter: #messageText:.
	morph acceptOnAny: true.
	morph openInWorld.
	"
	^ self
		textProvider: aTextProvider
		textGetter: getTextSel
		textSetter: setTextSel
		selectionGetter: nil! !

