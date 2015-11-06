'From Cuis 4.2 of 25 July 2013 [latest update: #2570] on 6 November 2015 at 3:38:58.925071 pm'!

!TheWorldMenu methodsFor: 'menu' stamp: 'dhn 11/3/2015 14:41'!
openCommentGuide
	"In ClassCommentBrowser.pck"
	Smalltalk at: #CommentGuideWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Class Comment Guide is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature require: #'ClassCommentBrowser'] 
	].
	Smalltalk at: #CommentGuide ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package ClassCommentBrowser.pck.st', String newLineString, 'Feature require: #''ClassCommentBrowser'' '! !


!TheWorldMenu methodsFor: 'construction' stamp: 'dhn 11/3/2015 14:39'!
helpMenu
	"Build the help menu for the world."
	| menu |
	menu _ self menu: 'Help...'.
	self
		fillIn: menu
		from: {
			{'About this System...'. {Smalltalk. #aboutThisSystem}. 'current version information.'}.
			{'Terse Guide to Cuis'. {self. #openTerseGuide}. 'explore Cuis Smalltalk'}.
			{'Class Comment Browser'. {self. #openCommentGuide}. 'search & explore Cuis Class Comments'}.
			{'Code management in Cuis'. {Utilities. #openCodeManagementInCuis}. 'current version information.'}.
			{'Using GitHub to host Cuis packages'. {Utilities. #openCuisAndGitHub}. 'current version information.'}.
			nil.
			{'Editor keyboard shortcuts'. {SmalltalkEditor. #openHelp}. 'summary of keyboard shortcuts in editors for Smalltalk code.'}
		}.
	self
		fillIn: menu
		from: {
			{'Useful Expressions'. {Utilities. #openUsefulExpressions}. 'a window full of useful expressions.'}.
			nil.
			{'VM Statistics'. {self. #vmStatistics}. 'obtain some intriguing data about the vm.'}.
			{'Space Left'. {self. #garbageCollect}. 'perform a full garbage-collection and report how many bytes of space remain in the image.'}
		}.
	^ menu! !

