'From Cuis 5.0 of 7 November 2016 [latest update: #3317] on 6 May 2018 at 11:55:36 am'!

!MVCMenuMorph class methodsFor: 'instance creation' stamp: 'jmv 5/6/2018 11:54:04'!
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
		menu add: (items at: i) target: menu action: #selectMVCItem: argument: (selections at: i).
		(lines includes: i) ifTrue: [menu addLine]].
	^ menu
! !

!methodRemoval: MenuMorph #add:selector:argument:!
MenuMorph removeSelector: #add:selector:argument:!
