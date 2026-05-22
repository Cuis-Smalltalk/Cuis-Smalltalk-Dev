"
ImageMorph is a morph that displays a picture (Form). My extent is determined by the extent of my form.

Use #image: to set my picture.

Structure:
 instance var		Type 		Description
 image				Form		The Form to use when drawing

Code examples:
	ImageMorph new openInWorld; grabFromScreen

	ImageMorph new image: (Form fromFileNamed: 'myGraphicsFileName'); openInWorld.

Relationship to SketchMorph: ImageMorph should be favored over SketchMorph, a parallel, legacy class -- see the Swiki FAQ for details ( http://minnow.cc.gatech.edu/squeak/1372 ). 

"
Class {
	#name : 'ImageMorph',
	#superclass : 'BorderedBoxMorph',
	#instVars : [
		'image'
	],
	#classVars : [
		'DefaultForm'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
ImageMorph class >> categoryInNewMorphMenu [
	^ 'Basic'
]

{ #category : 'cached state access' }
ImageMorph class >> defaultForm [
	"
	On an Inspector on the Form, do 
		self writeBMPfileNamed: 'DefaultForm.bmp'
		self writeJPEGfileNamed: 'DefaultForm.jpg'
	(Base64MimeConverter mimeEncode: ( 'DefaultForm.jpg' asFileEntry binaryContents readStream)) upToEnd edit
	Select all, copy, paste in the String literal in this method.
	ImageMorph defaultForm display
	"

	DefaultForm ifNotNil: [ ^DefaultForm ].
	DefaultForm := Form fromBinaryStream:
		'/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRof
Hh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwh
MjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjL/wAAR
CAA5AFUDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAA
AgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkK
FhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWG
h4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl
5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREA
AgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYk
NOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOE
hYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk
5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD3+iisbxP4ks/C+jvfXWHkJ2W9uGw08nZR
/MnsAT2oA5nxP8UbXwx4oOlXGntJbQxpJc3AnCugfONsePmHT+IHrgHu1fjL4UN2sBOoKu8I
8rWrBUyCQT39OACeRxjJHjHinxJHe64us67KjX0iCOMRR4WNAScADnALHlsnnrgYFe80qC5s
540jUSur7HPJVmOc5+tAH1PYX9pqljDfWNxHcWsy7o5Y2yrD/PFTySJFG0kjqiICzMxwAB1J
NfMfhrXNf0SF5bDWLq1WchzEu1ozwMkI4YKWIJJAB55NP1C9vtXZm1XULq/LNvK3MpaMN6rH
9xT/ALqigD3Sb4ieEYJmifX7QspwdhLD8CAQauaX4w8Oa1P5Gna1ZT3H/PETASHpyEOCRyOc
Yr55AAGAMCmyRpKu10DDryKAPqOivA/D/j/XvDgSIP8A2nYr/wAu11IQ6jGAEk5I7cEEcY4z
ket+GvGmj+KXnisZJY7mEBnt7hNj7f7w5IYZ4yCefqMgHQ0UUUAFfNviK/vr/wAWaodVdjfQ
zvEUIOIow3yIvGNuCD75z3r6SrjfF/w503xXcC8W4m06/wABZLi3APmqOgdTwSOx644zjigD
wyW3gnZGmhjkKHKF1B2n1GelSV0XjD4fa34VsItQt72PUbVFP2t/s/liHnhiN5IXHBbJx1Ix
05Lzb2QYS3ij/wBt5Nwx7ADn9KAJ5ZkhVS+fmZUAHUknFSVXhtdriWaQzSjozAAL/ujt/P3q
xQAUUUUAFTWmoXOkX9vqdmC1zaOJUUHG/HVeo4YZH41DRQB9NWV5b6jYW99aSCW2uYlmikAI
DIwyDz6giiub+Gs3nfD3SP8AWYiR4Bv64jkZBj2wvHtiigDq6KKKAEZVdSrAFSMEEcEV8za7
p58O6/qWkRxNLFZXBjTY2SsTKskY55JCOoPfIPWvpiSRIo2kkdURAWZmOAAOpJrwNLT/AITn
4n30cMhW3vLqSSSeEZAt4QsQdW5HzhEIJ4+fPtQBzMU0cy7o3DD9R9R2p9dV8Thoenavp9to
8US/YrFxciAbgQSvlKTzlwFfrz8wJzkV3WifC/w7aaRE2sWCXl+yB7medj8pxkqMHAVeQB+J
JJJIB43UYuIWnaASp5q8lM/Nj1x6V6RqNx8IJHuIlwjsxzcada3LKDnPyPGpQj6ZHavMtZm0
eXU7i10aO/1KziAKPdRKjh+cjdwOOMHCn2JGaALNM0qz1DVtWGj6apubmYZikYfLHyQTIQOF
X169RycZghvLZtL8pbC4i1G2QCa5F/5sHqTIpT5SFzwGHIHOM16J8MtF1+28Sw6lb2clrpNz
ETdyyIqi4XafL2g/NncwO4DBAOSeKAPWtH0yDRdGstMts+TaQrCpPVtoxk+56n3NFXaKACii
igDmvGPgy18a2dtZ3t/eW1tC5kZLYqPMOMDO5WHHUcflVTRPhtoWg2d7DbyX00t5ataS3U9w
TL5ZGCFIACnvkDORXYUUAef6b8HfDGmzxyK+ozrHL5qxzXPycNuC4AGQD/LnNd9JGksbRyIr
o4KsrDIIPUEU6igDn/8AhBfCO3b/AMItopXOQDYREA8dBt46CiTwL4SleNn8M6QfLBCr9jj2
9AOVxg8DjPTtXQUUAZ+maFo+ieb/AGTpVjYedjzPslukW/GcZ2gZxk9fU1oUUUAFFFFAH//Z'
				base64Decoded readStream.
	^DefaultForm.
]

{ #category : 'instance creation' }
ImageMorph class >> fromFileNamed: filename [
	^self newWith: (Form fromFileNamed: filename)
]

{ #category : 'instance creation' }
ImageMorph class >> new [
	^super new
		image: self defaultForm.
]

{ #category : 'instance creation' }
ImageMorph class >> newWith: aForm [
	^super new image: aForm
]

{ #category : 'cached state access' }
ImageMorph class >> releaseClassCachedState [
	DefaultForm := nil
]

{ #category : 'accessing' }
ImageMorph >> color: aColor [
        super color: aColor.
        (image depth = 1 and: [aColor is: #Color]) ifTrue: [
                image colors: {`Color transparent`. aColor}.
                self redrawNeeded]
]

{ #category : 'initialization' }
ImageMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 0
]

{ #category : 'drawing' }
ImageMorph >> drawOn: aCanvas [

	((padding > `0@0`) or: [self morphExtent > self minimumExtent]) 
		ifTrue: [ 	| innerOrigin |
			"Make use of my color in area between border and image"
			innerOrigin := borderWidth @ borderWidth.
			aCanvas
				fillRectangle:  (innerOrigin extent: extent - (2 * innerOrigin ))
				color:  color].
			
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle:  (`0@0` extent: extent)
			color:  borderColor
			borderWidth:  borderWidth 
			borderStyleSymbol:  #simple ].
		
	aCanvas image: image at: self interiorOrigin.

]

{ #category : 'accessing' }
ImageMorph >> form [
	^image
]

{ #category : 'menu commands' }
ImageMorph >> grabFromScreen [
	self image: Form fromUser
]

{ #category : 'accessing' }
ImageMorph >> image: anImage  [

	image := anImage depth = 1 
		ifTrue: [ ColorForm mappingWhiteToTransparentFrom: anImage ]
		ifFalse: [ anImage ].
	self morphExtent: self minimumExtent.
	self redrawNeeded.
]

{ #category : 'drawing' }
ImageMorph >> minimumExtent [

	^ image extent + (2 * self interiorOrigin)
]

{ #category : 'menu commands' }
ImageMorph >> readFromFile [
	self request: 'Enter file name'
		initialAnswer: 'fileName'
		do: [:fileName| self image: (Form fromFileNamed: fileName)]
]
