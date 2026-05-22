"
I model the requirements of a CodePackageList
"
Class {
	#name : 'PackageRequirementsList',
	#superclass : 'ActiveModel',
	#instVars : [
		'codePackageList',
		'requirements',
		'selection'
	],
	#category : 'Tools-Packages',
	#package : 'Tools',
	#tag : 'Packages'
}

{ #category : 'instance creation' }
PackageRequirementsList class >> fromCodePackageList: aCodePackageList [

	^ self new codePackageList: aCodePackageList 
]

{ #category : 'initialization' }
PackageRequirementsList >> codePackageList: cpList [

	codePackageList := cpList.
	selection := 0.
	self updateRequirementsFromPackageList.
]

{ #category : 'accessing' }
PackageRequirementsList >> deleteSelectedRequirement [

	| selectedPackage featureSpec |
	self selectionIndex ifNil: [ ^self ].
	self selectionIndex isZero ifTrue: [ ^self ].
	((codePackageList selectionIndex isNil)  or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ ^self ].
		
	selectedPackage := codePackageList selection.
	featureSpec := selectedPackage featureSpec.
	featureSpec 
		provides: featureSpec provides 
		requires: (featureSpec requires copyWithout: self selection).
	selectedPackage hasUnsavedChanges: true.
	requirements := codePackageList selection requires asArray.
	self changed: #requirements

	
	
]

{ #category : 'accessing' }
PackageRequirementsList >> requirementsStrings [

	^ (requirements collect: [ :req | req printString ]) asArray
]

{ #category : 'accessing' }
PackageRequirementsList >> selection [

	^selection
]

{ #category : 'accessing' }
PackageRequirementsList >> selectionIndex [

	^ requirements indexOf: selection
]

{ #category : 'accessing' }
PackageRequirementsList >> selectionIndex: anInteger [

	selection := anInteger = 0 ifFalse: [ requirements at: anInteger ].
]

{ #category : 'events' }
PackageRequirementsList >> updateRequirementsFromPackageList [
	
	| req |
	req := requirements.
	((codePackageList selectionIndex isNil) 
			or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ requirements := #() ]
		ifFalse: [ requirements := codePackageList selection requires asArray ].
		
	(req = requirements) ifFalse: [
		self changed: #requirements
	]
]

{ #category : 'accessing' }
PackageRequirementsList >> updateSelectedRequirement [

	| selectedPackage requiredPackage featureSpec requirementToUpdate updatedRequirement newRequires selectedName |
	self selectionIndex ifNil: [ ^self ].
	self selectionIndex isZero ifTrue: [ ^self ].
	((codePackageList selectionIndex isNil)  or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ ^self ].
		
	selectedPackage := codePackageList selection.
	featureSpec := selectedPackage featureSpec.
	requirementToUpdate := self selection.
	updatedRequirement := (selectedName := requirementToUpdate name) = Feature baseSystemFeature name
		ifTrue: [ Feature baseSystemFeature requirementOfMe ]
		ifFalse: [
			requiredPackage := CodePackage installedPackages at: selectedName.
			requiredPackage hasUnsavedChanges
				ifTrue: [ self notify: 'Please save package ', requiredPackage packageName, ' first. Requirement version of an unsaved package can not be updated.'. ^self ].
			requiredPackage requirementOfMe ].
	newRequires := (featureSpec requires copyWithout: requirementToUpdate) copyWith: updatedRequirement.
	featureSpec 
		provides: featureSpec provides 
		requires: newRequires.
	selectedPackage hasUnsavedChanges: true.
	requirements := codePackageList selection requires asArray.
	self changed: #requirements

	
	
]
