'From Cuis 5.0 of 7 November 2016 [latest update: #3296] on 1 May 2018 at 1:29:48 am'!
!classDefinition: #UpdatingMenuItemMorph category: #'Morphic-Menus'!
MenuItemMorph subclass: #UpdatingMenuItemMorph
	instanceVariableNames: 'wordingProvider wordingSelector enablementSelector wordingArgument '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

!UpdatingMenuItemMorph methodsFor: 'world' stamp: 'jmv 4/30/2018 21:36:20'!
updateContents
	"Update the receiver's contents"

	| newString enablement nArgs |
	((wordingProvider isNil) or: [wordingSelector isNil]) ifFalse: [
		nArgs _ wordingSelector numArgs.
		newString _ nArgs = 0
			ifTrue: [
				wordingProvider perform: wordingSelector]
			ifFalse: [
				nArgs = arguments size ifTrue: [
					wordingProvider perform: wordingSelector withArguments: arguments]].
		newString = (contentString ifNil: [ contents ])
			ifFalse: [
				self contents: newString ]].
	enablementSelector ifNotNil: [
		(enablement _ self enablement) == isEnabled 
			ifFalse:	[self isEnabled: enablement]]! !

!methodRemoval: UpdatingMenuItemMorph #wordingArgument:!
UpdatingMenuItemMorph removeSelector: #wordingArgument:!
!classDefinition: #UpdatingMenuItemMorph category: #'Morphic-Menus'!
MenuItemMorph subclass: #UpdatingMenuItemMorph
	instanceVariableNames: 'wordingProvider wordingSelector enablementSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
