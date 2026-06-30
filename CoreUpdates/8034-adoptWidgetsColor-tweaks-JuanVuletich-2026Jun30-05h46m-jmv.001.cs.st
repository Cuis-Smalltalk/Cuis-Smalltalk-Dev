'From Cuis7.9 [latest update: #8033] on 30 June 2026 at 5:49:09 am'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 30/Jun/2026, 9:40:41 am (UTC)'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	adoptsWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: paneColor) paler ]! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 30/Jun/2026, 9:40:53 am (UTC)'!
buildButtonPane

	| saveButton createButton deleteButton browseChangesButton browseButton addReqButton buttonRow |
	saveButton := PluggableButtonMorph 
							model: model 
							action: #save 
							label: 'save'.
	createButton := PluggableButtonMorph 
							model: self 
							action: #createPackage 
							label: 'new'.
	deleteButton := PluggableButtonMorph 
							model: self 
							action: #deletePackage 
							label: 'delete/merge'.
	browseChangesButton := PluggableButtonMorph 
							model: self 
							action: #browseChanges 
							label: 'changes'.
	browseButton := PluggableButtonMorph 
							model: self 
							action: #browse 
							label: 'browse'.
	addReqButton := PluggableButtonMorph 
							model: self 
							action: #addRequirement 
							label: 'add requirement'.
	buttonRow := LinearLayoutMorph newRow.
	buttonRow
		doAdoptWidgetsColor;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6;
		addMorph: addReqButton proportionalWidth: 0.6.
	buttonRow submorphsDo: [ :button | button  color: self widgetsColor ].
	^ buttonRow ! !

!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 30/Jun/2026, 9:44:03 am (UTC)'!
buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout updateReqButton |
	requirements := PluggableListMorph
		withModel: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
		
	deleteReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete')
							setBalloonText: 'Remove selected Feature requirement'.
	deleteReqButton color: self widgetsColor.
	updateReqButton := (PluggableButtonMorph 
							model: requirements model
							action: #updateSelectedRequirement 
							label: 'update')
							setBalloonText: 'Update requirement to current Feature revision'.
	updateReqButton color: self widgetsColor.
							
	buttonLayout := LinearLayoutMorph newRow.
	buttonLayout 
		addMorph: deleteReqButton 
		layoutSpec: (LayoutSizeSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop);
		addMorph: updateReqButton 
		layoutSpec: (LayoutSizeSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						offAxisEdgeWeight: #leftOrTop).
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LinearLayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSizeSpec 
							proportionalWidth: 0.8
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #leftOrTop);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSizeSpec 
							proportionalWidth: 0.2 
							proportionalHeight: 1.0 
							offAxisEdgeWeight: #rightOrBottom);
		color: `Color transparent`;
		yourself
		! !

