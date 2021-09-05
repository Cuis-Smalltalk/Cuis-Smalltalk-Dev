'From Haver 5.0 [latest update: #4821] on 1 September 2021 at 5:39:26 pm'!

!Boolean methodsFor: 'user interface support' stamp: 'KLG 9/1/2021 17:33:22'!
asMenuItemTextPrefix
	"Answer '<Yes>' or  '<No>' to prefix a menu item text with a check box. "

	^ self subclassResponsibility! !


!False methodsFor: 'user interface support' stamp: 'KLG 9/1/2021 17:33:51'!
asMenuItemTextPrefix
	"Answer '<Yes>' or  '<No>' to prefix a menu item text with a check box. "

	^ '<No>'! !


!True methodsFor: 'user interface support' stamp: 'KLG 9/1/2021 17:34:02'!
asMenuItemTextPrefix
	"Answer '<Yes>' or  '<No>' to prefix a menu item text with a check box. "

	^ '<Yes>'! !


!Boolean reorganize!
('logical operations' & | eqv: not xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or:)
('copying' shallowCopy)
('testing' is:)
('printing' isLiteral storeOn:)
('user interface support' asMenuItemTextPrefix)
!


!False reorganize!
('logical operations' & not xor: |)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('printing' printOn:)
('user interface support' asMenuItemTextPrefix)
!


!True reorganize!
('logical operations' & not xor: |)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('printing' printOn:)
('user interface support' asMenuItemTextPrefix)
!

