'From Cuis 5.0 of 7 November 2016 [latest update: #3282] on 27 March 2018 at 5:11:12 pm'!

!InspectorWindow methodsFor: 'accessing' stamp: 'HAW 3/27/2018 17:07:55'!
classDefinitionChangedFrom: oldClass to: newClass

	model ifNotNil: [ model object class = newClass ifTrue: [ model changed: #fieldList ]]! !

!InspectorWindow methodsFor: 'accessing' stamp: 'HAW 3/27/2018 17:07:24'!
model: aModel

	super model: aModel.
	model ifNotNil: [ 
		SystemChangeNotifier uniqueInstance
			when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self ]
! !

