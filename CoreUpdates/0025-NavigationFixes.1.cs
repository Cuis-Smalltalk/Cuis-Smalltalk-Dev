'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 20 May 2008 at 8:00:21 pm'!!LightWidget methodsFor: 'add/remove' stamp: 'jmv 5/20/2008 19:58'!delete	"Remove the receiver as a submorph of its owner and make its 	new owner be nil."	| aWorld |	aWorld := self world ifNil: [World].	"Terminate genie recognition focus"	"I encountered a case where the hand was nil, so I put in a little 	protection - raa "	" This happens when we are in an MVC project and open	  a morphic window. - BG "	aWorld ifNotNil:[		self activeHand			releaseKeyboardFocus: self;			releaseMouseFocus: self;			releaseNavigationFocus: self].	owner ifNotNil:[ self privateDelete].! !!Morph methodsFor: 'No se...' stamp: 'jmv 5/20/2008 19:57'!delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld := self world ifNil: [World].
	"Terminate genie recognition focus"
	"I encountered a case where the hand was nil, so I put in a little 
	protection - raa "
	" This happens when we are in an MVC project and open
	  a morphic window. - BG "
	aWorld ifNotNil: [		self activeHand			releaseKeyboardFocus: self;			releaseMouseFocus: self;			releaseNavigationFocus: self].
	owner ifNotNil:[ self privateDelete].! !!OldMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 5/20/2008 19:59'!delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld := self world ifNil: [World].
	"Terminate genie recognition focus"
	"I encountered a case where the hand was nil, so I put in a little 
	protection - raa "
	" This happens when we are in an MVC project and open
	  a morphic window. - BG "
	aWorld ifNotNil: [
	 	self disableSubmorphFocusForHand: self activeHand.		self activeHand			releaseKeyboardFocus: self;			releaseMouseFocus: self;			releaseNavigationFocus: self].
	owner ifNotNil:[ self privateDelete].! !!OldHandMorph methodsFor: 'focus handling' stamp: 'jmv 5/20/2008 19:55'!releaseAllFoci	mouseFocus _ nil.	keyboardFocus _ nil.	navigationFocus _ nil! !!OldHandMorph methodsFor: 'focus handling' stamp: 'jmv 5/20/2008 19:56'!releaseNavigationFocus: aMorph	"If the given morph had the navigation focus before, release it"	navigationFocus == aMorph ifTrue:[self navigationFocus: nil].! !