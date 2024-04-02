'From Cuis6.3 [latest update: #6304] on 2 April 2024 at 9:46:36 am'!

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 4/2/2024 09:14:06'!
copySelection
	"Copy the current selection and store it in the Clipboard, unless empty."

	| multiSelection |

	"Copying whitespace to clipboard is OK."
	(self hasSelection and: [self selection isSeparators]) ifFalse: [
		self lineSelectAndEmptyCheck: [^ self]].

	multiSelection := self selection.
	self clipboardTextPut: multiSelection! !

