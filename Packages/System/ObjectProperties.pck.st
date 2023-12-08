'From Cuis6.0 [latest update: #6114] on 8 December 2023 at 11:09:01 am'!
'Description '!
!provides: 'ObjectProperties' 1 1!
SystemOrganization addCategory: #ObjectProperties!


!classDefinition: #ObjectWithProperties category: #ObjectProperties!
Object subclass: #ObjectWithProperties
	instanceVariableNames: 'propertyMap'
	classVariableNames: 'PropertyMaps'
	poolDictionaries: ''
	category: 'ObjectProperties'!
!classDefinition: 'ObjectWithProperties class' category: #ObjectProperties!
ObjectWithProperties class
	instanceVariableNames: ''!


!ObjectWithProperties methodsFor: 'accessing' stamp: 'jmv 11/16/2023 10:48:19'!
propertyMap
	propertyMap ifNil: [
		propertyMap := IdentityDictionary new ].
	^propertyMap! !

!ObjectWithProperties methodsFor: 'accessing' stamp: 'jmv 11/16/2023 10:47:54'!
propertyMapOrNil
	^propertyMap! !

!ObjectWithProperties methodsFor: 'accessing' stamp: 'jmv 11/16/2023 10:47:02'!
releasePropertyMap
	propertyMap := nil! !

!ObjectWithProperties methodsFor: 'copying' stamp: 'jmv 11/16/2023 10:50:19'!
postCopy
	propertyMap := nil! !

!ObjectWithProperties class methodsFor: 'accessing' stamp: 'jmv 11/16/2023 10:43:01'!
propertyMapFor: anObject

	^self propertyMaps
		at: anObject
		ifAbsentPut: [ IdentityDictionary new ]! !

!ObjectWithProperties class methodsFor: 'accessing' stamp: 'jmv 11/16/2023 10:43:04'!
propertyMapOrNilFor: anObject

	^self propertyMaps
		at: anObject
		ifAbsent: nil! !

!ObjectWithProperties class methodsFor: 'accessing' stamp: 'jmv 11/16/2023 10:43:09'!
propertyMaps

	PropertyMaps ifNil: [
		PropertyMaps := WeakIdentityKeyDictionary new ].
	^PropertyMaps! !

!ObjectWithProperties class methodsFor: 'releasing' stamp: 'jmv 11/16/2023 10:43:14'!
releasePropertyMapFor: anObject

	self propertyMaps
		removeKey: anObject
		ifAbsent: nil! !

!Object methodsFor: '*ObjectProperties-querying / testing' stamp: 'jmv 11/16/2023 11:02:59'!
hasProperty: aSymbol

	^self propertyMapOrNil
		ifNil: false
		ifNotNil: [ :map | map includesKey: aSymbol ]! !

!Object methodsFor: '*ObjectProperties-querying / testing' stamp: 'jmv 11/16/2023 11:09:09'!
propertyAt: aSymbol

	^self propertyMapOrNil ifNotNil: [ :map |
		map at: aSymbol ifAbsent: nil ]! !

!Object methodsFor: '*ObjectProperties-querying / testing' stamp: 'jmv 11/16/2023 10:55:25'!
propertyAt: aSymbol ifAbsent: absentBlock

	^self propertyMapOrNil
		ifNil: absentBlock
		ifNotNil: [ :map |
			map at: aSymbol ifAbsent: absentBlock ]! !

!Object methodsFor: '*ObjectProperties-adding / removing' stamp: 'jmv 12/8/2023 09:01:05'!
propertyAt: aSymbol ifAbsentPut: aBlock
	^self propertyMap at: aSymbol ifAbsentPut: aBlock! !

!Object methodsFor: '*ObjectProperties-querying / testing' stamp: 'jmv 11/16/2023 11:15:47'!
propertyAt: aSymbol ifPresentDo: presentBlock

	^self propertyMapOrNil
		ifNotNil: [ :map |
			map at: aSymbol ifPresent: presentBlock ]! !

!Object methodsFor: '*ObjectProperties-adding / removing' stamp: 'jmv 11/16/2023 11:04:24'!
propertyAt: aSymbol put: propertyValue
	^self propertyMap at: aSymbol put: propertyValue! !

!Object methodsFor: '*ObjectProperties-private' stamp: 'jmv 11/16/2023 10:45:17'!
propertyMap
	^ObjectWithProperties propertyMapFor: self! !

!Object methodsFor: '*ObjectProperties-private' stamp: 'jmv 11/16/2023 10:47:50'!
propertyMapOrNil
	^ObjectWithProperties propertyMapOrNilFor: self! !

!Object methodsFor: '*ObjectProperties-enumerating' stamp: 'jmv 11/16/2023 11:23:35'!
propertyNamesAndValuesDo: aBlock

	^self propertyMapOrNil
		ifNotNil: [ :map | map keysAndValuesDo: aBlock ]! !

!Object methodsFor: '*ObjectProperties-enumerating' stamp: 'jmv 11/16/2023 11:05:50'!
propertyNamesDo: aBlock

	^self propertyMapOrNil
		ifNotNil: [ :map | map keysDo: aBlock ]! !

!Object methodsFor: '*ObjectProperties-private' stamp: 'jmv 11/16/2023 10:49:40'!
releasePropertyMap
	ObjectWithProperties releasePropertyMapFor: self! !

!Object methodsFor: '*ObjectProperties-adding / removing' stamp: 'jmv 11/16/2023 11:21:32'!
removeProperty: aSymbol
	"removes the property named aSymbol if it exists"

	| map |
	map := self propertyMapOrNil.
	map ifNil: [ ^self ].
	map removeKey: aSymbol ifAbsent: nil.
	map isEmpty ifTrue: [
		self releasePropertyMap ].! !
