'From Cuis 5.0 of 7 November 2016 [latest update: #3025] on 10 January 2017 at 8:09:22 pm'!

!SmalltalkEditor methodsFor: 'editing keys' stamp: 'HernanWilkinson 1/10/2017 20:04:06'!
acceptAndTest: aKeyboardEvent

	^self acceptAndTest! !

!SmalltalkEditor methodsFor: 'editing keys' stamp: 'HernanWilkinson 1/10/2017 20:04:34'!
acceptAndTestAll: aKeyboardEvent

	^self acceptAndTestAll! !

!SmalltalkEditor methodsFor: 'editing keys' stamp: 'HernanWilkinson 1/10/2017 20:02:27'!
debugIt: aKeyboardEvent

	self debugIt.
	^true! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'HernanWilkinson 1/10/2017 20:03:23'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$t	#acceptAndTest:				'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:				'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:								'Print it (selection is a valid expression)')

		#(		$s	#save:								'Save (i.e. accept)')
		#(		$d	#doIt:								'Do it (selection is a valid expression)')
		#(		$l	#cancelEdits:						'Cancel')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:						'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:				'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingit:'Method strings containing it')
		#(		$T	#displayIfTrue:					'Insert #ifTrue:')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$A	#argAdvance:						'Advance argument')
		#(		$D	#debugIt:							'Debut it')
		#(		$F	#displayIfFalse:					'Insert #ifFalse:')
		#(		$G	#fileItIn:							'File in selection')

		#(		$V	#pasteInitials:					'Paste author initials')
		#(		$N	#referencesToIt:					'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !

!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'HernanWilkinson 1/10/2017 19:58:01'!
initializeMenu
	"Initialize the mouseButton2 (right button) pop-up menu and corresponding messages."
	"
	Editor initialize
	"

	menu _ SelectionMenu fromArray: {
		{'Help...'.										#openHelp}.
		#-.
		{'Do it (d)'.									#doIt}.
		{'Print it (p)'.									#printIt}.
		{'Inspect it (i)'.								#inspectIt}.
		{'Explore it (I)'.								#exploreIt}.
		{'Debug it (D)'.								#debugIt}.
		{'Profile it'.									#profileIt}.
		#-.
		{'Explain'.										#explain}.
		{'Browse it (b)'.								#browseIt}.
		{'Senders of it (n)'.							#sendersOfIt}.
		{'Implementors of it (m)'.					#implementorsOfIt}.
		{'References to it (N)'.						#referencesToIt}.
		#-.
		{'Method Strings with it (E)'.				#methodStringsContainingit}.
		{'Method Source with it'.					#methodSourceContainingIt}.
		{'Class Comments with it'.					#classCommentsContainingIt}.
		#-.
		{'Accept (s)'.									#acceptContents}.
		{'Cancel (l)'.									#cancelEdits}.
		{'Accept & Run Test in Class (t)'.			#acceptAndTest}.
		{'Accept & Run Test in Category (y)'.		#acceptAndTestAll}.
		#-.
		{'More...'.										#getMenu2}.
	}.
	menu2 _ SelectionMenu fromArray: {
		{'Find...(f)'.									#find}.
		{'Find Again (g)'.								#findAgain}.
		{'Use Selection for Find (j)'.				#setSearchString}.
		#-.
		{'Undo - multiple (z)'.						#undo}.
		{'Redo - multiple (Z)'.						#redo}.
		{'Undo / Redo history'.						#offerUndoHistory}.
		#-.
		{'Copy (c)'.									#copySelection}.
		{'Cut (x)'.										#cut}.
		{'Paste (v)'.									#paste}.
		{'Paste without Format'.						#pasteString}.
		{'Paste...'.									#pasteRecent}.
		#-.
		{'Toggle WordWrap'.						#wrapOnOff}.
		{'Set Font... (k)'.								#offerFontMenu}.
		{'Clear Font'.									#clearFont}.
		{'Set Default Font...'.						#offerDefaultFontMenu}.
		#-.
		{'More...'.										#getMenu}.
	}! !


!InnerTextMorph methodsFor: 'editing' stamp: 'HernanWilkinson 1/10/2017 19:46:39'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| accepted prevSelection prevScrollValue |
	
	prevSelection _ self editor selectionInterval copy.
	prevScrollValue _ owner verticalScrollBar value.
	
	(self canDiscardEdits and: [(self hasProperty: #alwaysAccept) not]) ifTrue: [self flash. ^true].
	(self hasEditingConflicts and: [ self confirmAcceptAnyway not ]) ifTrue: [self flash. ^false].
	
	accepted _ model acceptContentsFrom: owner.
	"During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the code-holding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
	
	"some implementors of acceptContentsFrom: answer self :("
	^accepted == true 
		ifTrue: [
			model refetch.
			self editor selectFrom: prevSelection first to: prevSelection last.
			WorldState addDeferredUIMessage: [
				self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
				owner verticalScrollBar setValue: prevScrollValue].
			true]
		ifFalse: [ false ]! !

Editor initialize!

