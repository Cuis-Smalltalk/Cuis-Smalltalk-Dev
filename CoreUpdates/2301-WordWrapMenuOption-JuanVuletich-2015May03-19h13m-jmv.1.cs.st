'From Cuis 4.2 of 25 July 2013 [latest update: #2299] on 3 May 2015 at 7:22:58.184583 pm'!

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 5/3/2015 19:21'!
wrapOnOff
	"Toggle WordWrap"

	morph wrapOnOff! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 5/3/2015 19:20'!
initializeMenu
	"Initialize the mouseButton2 (right button) pop-up menu and corresponding messages."
	"
	Editor initialize
	"

	menu _ SelectionMenu fromArray: {
		{'Help...'.						#openHelp}.
		#-.
		{'Find...(f)'.						#find}.
		{'Find Again (g)'.				#findAgain}.
		{'Use Selection for Find (j)'.		#setSearchString}.
		#-.
		{'Undo - multiple (z)'.			#undo}.
		{'Redo - multiple (Z)'.			#redo}.
		{'Undo / Redo history'.			#offerUndoHistory}.
		#-.
		{'Copy (c)'.						#copySelection}.
		{'Cut (x)'.						#cut}.
		{'Paste (v)'.						#paste}.
		{'Paste without Format'.		#pasteString}.
		{'Paste...'.						#pasteRecent}.
		#-.
		{'Toggle WordWrap'.			#wrapOnOff}.
		{'Set Font... (k)'.					#offerFontMenu}.
		{'Set Alignment...'.				#chooseAlignment}.
	}! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 5/3/2015 19:20'!
initializeMenu
	"Initialize the mouseButton2 (right button) pop-up menu and corresponding messages."
	"
	Editor initialize
	"

	menu _ SelectionMenu fromArray: {
		{'Help...'.						#openHelp}.
		#-.
		{'Find...(f)'.						#find}.
		{'Find Again (g)'.				#findAgain}.
		{'Use Selection for Find (j)'.		#setSearchString}.
		#-.
		{'Undo - multiple (z)'.			#undo}.
		{'Redo - multiple (Z)'.			#redo}.
		{'Undo / Redo history'.			#offerUndoHistory}.
		#-.
		{'Copy (c)'.						#copySelection}.
		{'Cut (x)'.						#cut}.
		{'Paste (v)'.						#paste}.
		{'Paste without Format'.		#pasteString}.
		{'Paste...'.						#pasteRecent}.
		#-.
		{'Accept (s)'.					#acceptContents}.
		{'Cancel (l)'.					#cancelEdits}.
		#-.
		{'Toggle WordWrap'.			#wrapOnOff}.
		{'Smalltalk Options'.			#getMenu2}.
	}.
	menu2 _ SelectionMenu fromArray: {
		{'Do it (d)'.						#doIt}.
		{'Print it (p)'.					#printIt}.
		{'Inspect it (i)'.					#inspectIt}.
		{'Explore it (I)'.					#exploreIt}.
		{'Debug it'.						#debugIt}.
		#-.
		{'Explain'.						#explain}.
		{'Browse it (b)'.					#browseIt}.
		{'Senders of it (n)'.				#sendersOfIt}.
		{'Implementors of it (m)'.		#implementorsOfIt}.
		{'References to it (N)'.			#referencesToIt}.
		#-.
		{'Method Strings with it (E)'.	#methodStringsContainingit}.
		{'Method Source with it'.		#methodSourceContainingIt}.
		{'Class Comments with it'.		#classCommentsContainingIt}.
	}! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 5/3/2015 19:22'!
wrapFlag: aBoolean
	"Change whether contents are wrapped to the container."

	aBoolean == wrapFlag ifTrue: [^ self].
	wrapFlag _ aBoolean.

	"Compose my text to fit my bounds."
	self resetTextComposition.
	self editor recomputeSelection.	
	self updateFromTextComposition ! !

