'From Cuis7.1 [latest update: #6737] on 20 September 2024 at 4:24:51 pm'!

!SystemDictionary methodsFor: 'browsing - main' stamp: 'jmv 9/20/2024 16:24:14'!
browse
	"
	Smalltalk browse
	"
	^self browserWindowClass fullOnClass: Object selector: nil! !


!BrowserWindow class methodsFor: 'menu-world' stamp: 'jmv 9/20/2024 16:24:24'!
worldMenuOptions
	^ `{{
			#submenuOf 			-> 		TheWorldMenu openLabel.
			#itemGroup 			-> 		10.
			#itemOrder 			-> 		30.
			#label 			-> 		'Browser'.
			#object 			-> 		Smalltalk.
			#selector 			-> 		#browse.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 			-> 		'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary}`! !

