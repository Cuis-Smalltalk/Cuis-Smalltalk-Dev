Class {
	#name : 'FeatureChangeRecord',
	#superclass : 'ChangeListElement',
	#instVars : [
		'type',
		'feature'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'accessing' }
FeatureChangeRecord >> changeClass [
	^nil
]

{ #category : 'accessing' }
FeatureChangeRecord >> changeClassName [
	^nil
]

{ #category : 'accessing' }
FeatureChangeRecord >> changeType [
	^ type
]

{ #category : 'accessing' }
FeatureChangeRecord >> feature [
	^feature
]

{ #category : 'fileIn/fileOut' }
FeatureChangeRecord >> fileIn [
	
	"It does nothing - Hernan"
]

{ #category : 'fileIn/fileOut' }
FeatureChangeRecord >> fileOutOn: aFileStream [

	"Nothing to file out - Hernan"
]

{ #category : 'accessing' }
FeatureChangeRecord >> string [
	^ type, ': ', feature printString
]

{ #category : 'accessing' }
FeatureChangeRecord >> type: aSymbol feature: aFeature [
	type := aSymbol.
	feature := aFeature
]
