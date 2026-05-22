"
This class just packages two pieces of information useful for packages:

	provides  -- the Feature which our containing package supplies
	requires -- nil or a Set of Features required by our containing package
"
Class {
	#name : 'FeatureSpec',
	#superclass : 'Object',
	#instVars : [
		'provides',
		'requires'
	],
	#category : 'System-Package Support',
	#package : 'System',
	#tag : 'Package Support'
}

{ #category : 'instance creation' }
FeatureSpec class >> provides: providedFeature requires: requiredFeaturesSet [

	^self new provides: providedFeature requires: requiredFeaturesSet asSet
]

{ #category : 'initialization' }
FeatureSpec >> initialize [
	requires := Set new
]

{ #category : 'accessing' }
FeatureSpec >> name [
	"Answer the name of the Feature provided"

	^provides name
]

{ #category : 'printing' }
FeatureSpec >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPut: $(.
	provides printOn: aStream.
	aStream nextPutAll: ' requires: '.
	requires printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'accessing' }
FeatureSpec >> provides [

	^ provides
]

{ #category : 'accessing' }
FeatureSpec >> provides: aFeature [

	provides := aFeature
]

{ #category : 'initialization' }
FeatureSpec >> provides: providesFeatureSpec requires: setOfRequiresFeatureSpec [

	provides := providesFeatureSpec.
	requires := setOfRequiresFeatureSpec.
]

{ #category : 'accessing' }
FeatureSpec >> requirementOfMe [
	"Answer a FeatureRequirement with suitable defaults"
	
	^ self provides requirementOfMe
]

{ #category : 'accessing' }
FeatureSpec >> requires [

	^ requires
]

{ #category : 'accessing' }
FeatureSpec >> requires: aFeature [

	requires add: aFeature
]
