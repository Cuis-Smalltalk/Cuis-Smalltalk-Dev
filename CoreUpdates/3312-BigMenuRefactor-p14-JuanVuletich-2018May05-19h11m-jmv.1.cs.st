'From Cuis 5.0 of 7 November 2016 [latest update: #3311] on 5 May 2018 at 7:22:00 pm'!
!methodRemoval: SelectionMenu #selections!
SelectionMenu removeSelector: #selections!
!methodRemoval: MenuMorph #addCustomMenuItems:hand:!
MenuMorph removeSelector: #addCustomMenuItems:hand:!
!methodRemoval: MenuMorph #addItem!
MenuMorph removeSelector: #addItem!
!methodRemoval: MenuMorph #addTitle!
MenuMorph removeSelector: #addTitle!
!methodRemoval: MenuMorph #sightTarget:!
MenuMorph removeSelector: #sightTarget:!

!SelectionMenu reorganize!
('accessing' selections:)
('basic control sequence' startUpWithCaption:at:allowKeyboard:)
!

!classRemoval: #CustomMenu!
Smalltalk removeClassNamed: #CustomMenu!
