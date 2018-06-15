'From Cuis 5.0 of 7 November 2016 [latest update: #3345] on 14 June 2018 at 12:30:21 pm'!
!classDefinition: #PopUpMenu category: #'Tools-Menus'!
Object subclass: #PopUpMenu
	instanceVariableNames: 'labelString lineArray icons '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Menus'!

!MenuMorph methodsFor: 'construction' stamp: 'HAW 6/14/2018 12:12:12'!
add: aString target: target action: aSymbol argument: arg icon: symbolOrFormOrNil
	
	^(self add: aString
		target: target
		action: aSymbol
		argumentList: { arg }) 
			setIcon: symbolOrFormOrNil;
			yourself
! !


!PopUpMenu methodsFor: 'accessing' stamp: 'HAW 6/14/2018 12:13:38'!
iconAt: aPosition

	^icons at: aPosition ifAbsent: [ nil ]! !

!PopUpMenu methodsFor: 'private' stamp: 'HAW 6/14/2018 11:56:15'!
labels: aString lines: anArray icons: iconCollection

	labelString _ aString.
	lineArray _ anArray.
	icons _ iconCollection 
! !


!PopUpMenu class methodsFor: 'instance creation' stamp: 'HAW 6/14/2018 12:25:34'!
labelArray: labelArray lines: lineArray icons: icons
	"Answer an instance of me whose items are in labelArray, with lines 
	drawn after each item indexed by anArray. 2/1/96 sw"

	labelArray isEmpty ifTrue: [self error: 'Menu must not be zero size'].
	^ self
		labels: (String streamContents: 
			[:stream |
			labelArray do: [:each | stream nextPutAll: each; newLine].
			stream skip: -1 "remove last newline"])
		lines: lineArray
		icons: icons

"Example:
	(PopUpMenu labelArray: #('frog' 'and' 'toad') lines: #() icons: #()) startUpWithCaption: 'Please pick one.'
"! !

!PopUpMenu class methodsFor: 'instance creation' stamp: 'HAW 6/14/2018 12:04:54'!
labels: aString lines: anArray icons: icons
	"Answer an instance of me whose items are in aString, with lines drawn 
	after each item indexed by anArray and icons per item."

	^ self new labels: aString lines: anArray icons: icons! !

!PopUpMenu class methodsFor: 'dialogs' stamp: 'HAW 6/14/2018 12:21:38'!
confirm: queryString trueChoice: trueChoice falseChoice: falseChoice icons: icons
	"Put up a yes/no menu with caption queryString. The actual wording 
	for the two choices will be as provided in the trueChoice and 
	falseChoice parameters. Answer true if the response is the true-choice, 
	false if it's the false-choice.
	This is a modal question -- the user must respond one way or the other."

	"PopUpMenu confirm: 'Are you hungry?' trueChoice: 'yes, I''m famished' falseChoice: 'no, I just ate'"

	"PopUpMenu  confirm: 'Are you hungry?' trueChoice: 'yes, I''m famished' falseChoice: 'no, I just ate' icons: #(acceptIcon cancelIcon)"

	| menu choice |
	menu _ self labelArray: {trueChoice. falseChoice} lines: nil icons: icons.
	[(choice _ menu startUpWithCaption: queryString) isNil] whileTrue.
	^ choice = 1! !


!MVCMenuMorph class methodsFor: 'instance creation' stamp: 'HAW 6/14/2018 12:13:56'!
from: aPopupMenu title: titleStringOrNil
	"Answer a MenuMorph constructed from the given PopUpMenu. Used to simulate MVC-style menus in a Morphic-only world."

	| menu items lines selections labelString j emphasis |
	menu _ self new.
	titleStringOrNil ifNotNil: [
		titleStringOrNil isEmpty ifFalse: [menu addTitle: titleStringOrNil]].
	labelString _ aPopupMenu labelString.
	items _ labelString asString lines.
	(labelString is: #Text) ifTrue: [
		"Pass along text emphasis if present"
		j _ 1.
		items _ items collect: [ :item |
			j _ labelString asString findString: item startingAt: j.
			emphasis _ TextEmphasis new emphasisCode: (labelString emphasisAt: j).
			item asText addAttribute: emphasis]].
	lines _ aPopupMenu lineArray.
	lines ifNil: [lines _ #()].
	menu cancelValue: 0.
	menu defaultTarget: menu.
	selections _ (1 to: items size) asArray.
	1 to: items size do: [ :i |
		menu add: (items at: i) target: menu action: #selectMVCItem: argument: (selections at: i) icon: (aPopupMenu iconAt: i).
		(lines includes: i) ifTrue: [menu addLine]].
	^ menu
! !


!PopUpMenu methodsFor: 'private' stamp: 'HAW 6/14/2018 11:56:37'!
labels: aString lines: anArray

	self labels: aString lines: anArray icons: #()! !


!PopUpMenu class methodsFor: 'instance creation' stamp: 'HAW 6/14/2018 12:02:45'!
labelArray: labelArray lines: lineArray
	
	^self labelArray: labelArray lines: lineArray icons: #()! !

!PopUpMenu class methodsFor: 'instance creation' stamp: 'HAW 6/14/2018 12:04:21'!
labels: aString lines: anArray
	"Answer an instance of me whose items are in aString, with lines drawn 
	after each item indexed by anArray."

	^ self labels: aString lines: anArray icons: #()! !

!PopUpMenu class methodsFor: 'dialogs' stamp: 'HAW 6/14/2018 12:18:59'!
confirm: queryString
	"Put up a yes/no menu with caption queryString. Answer true if the 
	response is yes, false if no. This is a modal question--the user must 
	respond yes or no."

	"
	PopUpMenu confirm: 'Are you hungry?'
	"

	^ self confirm: queryString trueChoice: 'Yes' falseChoice: 'No' icons: #(acceptIcon cancelIcon)! !

!PopUpMenu class methodsFor: 'dialogs' stamp: 'HAW 6/14/2018 12:17:00'!
confirm: queryString orCancel: cancelBlock
	"Put up a yes/no/cancel menu with caption aString. Answer true if  
	the response is yes, false if no. If cancel is chosen, evaluate  
	cancelBlock. This is a modal question--the user must respond yes or no."

	"PopUpMenu confirm: 'Reboot universe' orCancel: [^'Nevermind']"

	| menu choice |
	menu _ self labelArray: {'Yes'. 'No'. 'Cancel'} lines: #() icons: #(acceptIcon cancelIcon collapseIcon).
	choice _ menu startUpWithCaption: queryString.
	choice = 1 ifTrue: [^ true].
	choice = 2 ifTrue: [^ false].
	^ cancelBlock value! !

!PopUpMenu class methodsFor: 'dialogs' stamp: 'HAW 6/14/2018 12:22:28'!
confirm: queryString trueChoice: trueChoice falseChoice: falseChoice
	
	"See #confirm:trueChoice:falseChoice:icons:"

	^self confirm: queryString trueChoice: trueChoice falseChoice: falseChoice icons: #()! !

!methodRemoval: PopUpMenu #icons!
PopUpMenu removeSelector: #icons!
!classDefinition: #PopUpMenu category: #'Tools-Menus'!
Object subclass: #PopUpMenu
	instanceVariableNames: 'labelString lineArray icons'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Menus'!
