'From Cuis 4.2 of 25 July 2013 [latest update: #2957] on 25 October 2016 at 12:20:13 am'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'pb 10/25/2016 00:15:24'!
profileIt
	"Treat the current selection as an expression; profile it
	3 +4"
	| provider rcvr ctxt |
	self lineSelectAndEmptyCheck: [^ ''].

	provider := self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
				rcvr := provider doItReceiver.
				ctxt := provider doItContext]
		ifFalse: [rcvr := ctxt := nil].
	[
		"A bit of a hack needed due to string injection below to prevent the injected expression from being invisible to other browsers etc."
		true ifFalse: [AndreasSystemProfiler spyOn: [nil]].
		Compiler new
			sourceStreamGetter: #selectionAsStream;		"Cuis specific. Do not remove!!"
			evaluate: ((ReadWriteStream on: '')
				nextPutAll: 'AndreasSystemProfiler spyOn: [';
				nextPutAll: self selection;
				nextPut: $];
				position: 0;
				yourself)
			in: ctxt
			to: rcvr
			notifying: self
			ifFail: [self error: 'Could not profile expression']
			logged: true.
	] 
		on: OutOfScopeNotification 
		do: [ :ex | ex resume: true].
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'pb 10/25/2016 00:18:53'!
initializeMenu
	"Initialize the mouseButton2 (right button) pop-up menu and corresponding messages."
	"
	Editor initialize
	"

	menu := SelectionMenu fromArray: {
		{'Help...'.							#openHelp}.
		#-.
		{'Do it (d)'.						#doIt}.
		{'Print it (p)'.						#printIt}.
		{'Inspect it (i)'.					#inspectIt}.
		{'Explore it (I)'.					#exploreIt}.
		{'Debug it'.						#debugIt}.
		{'Profile it'.						#profileIt}.
		#-.
		{'Explain'.							#explain}.
		{'Browse it (b)'.					#browseIt}.
		{'Senders of it (n)'.				#sendersOfIt}.
		{'Implementors of it (m)'.		#implementorsOfIt}.
		{'References to it (N)'.			#referencesToIt}.
		#-.
		{'Method Strings with it (E)'.	#methodStringsContainingit}.
		{'Method Source with it'.		#methodSourceContainingIt}.
		{'Class Comments with it'.		#classCommentsContainingIt}.
		#-.
		{'Accept (s)'.						#acceptContents}.
		{'Cancel (l)'.						#cancelEdits}.
		#-.
		{'More...'.							#getMenu2}.
	}.
	menu2 := SelectionMenu fromArray: {
		{'Find...(f)'.						#find}.
		{'Find Again (g)'.					#findAgain}.
		{'Use Selection for Find (j)'.	#setSearchString}.
		#-.
		{'Undo - multiple (z)'.			#undo}.
		{'Redo - multiple (Z)'.			#redo}.
		{'Undo / Redo history'.			#offerUndoHistory}.
		#-.
		{'Copy (c)'.						#copySelection}.
		{'Cut (x)'.							#cut}.
		{'Paste (v)'.						#paste}.
		{'Paste without Format'.			#pasteString}.
		{'Paste...'.						#pasteRecent}.
		#-.
		{'Toggle WordWrap'.			#wrapOnOff}.
		{'Set Font... (k)'.					#offerFontMenu}.
		{'Clear Font'.						#clearFont}.
		{'Set Default Font...'.			#offerDefaultFontMenu}.
		#-.
		{'More...'.							#getMenu}.
	}! !

Editor initialize!
