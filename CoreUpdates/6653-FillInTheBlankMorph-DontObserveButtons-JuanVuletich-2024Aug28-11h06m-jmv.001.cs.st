'From Cuis7.1 [latest update: #6646] on 28 August 2024 at 11:09:20 am'!

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/28/2024 11:07:27'!
createAcceptButton
	"create the [accept] button"
	| result |
	result := PluggableButtonMorph new
		 actionTarget: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	result morphExtent: `6@2` * self sizeUnit.
	self addMorph: result position: `2@7.5` * self sizeUnit // 1.
	^ result! !

!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 8/28/2024 11:07:31'!
createCancelButton
	"create the [cancel] button"
	| result |
	result := PluggableButtonMorph new
		 actionTarget: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	result morphExtent: `6@2` * self sizeUnit.
	self addMorph: result position: `12@7.5` * self sizeUnit // 1.
	^ result! !

