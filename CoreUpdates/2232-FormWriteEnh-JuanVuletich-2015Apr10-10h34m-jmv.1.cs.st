'From Cuis 4.2 of 25 July 2013 [latest update: #2232] on 10 April 2015 at 6:09:51.141219 pm'!

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 4/10/2015 17:21'!
writeFileNamed: filename 
	" 
	Display writeFileNamed: 'pp.bmp'
	Display writeFileNamed: 'pp.jpg'
	Display writeFileNamed: 'pp.png'
	Display writeFileNamed: 'ppp'
	"
	ImageReadWriter write: self onFileNamed: filename! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 4/10/2015 17:13'!
write: aForm onFileNamed: filename

	(self subclassFor: (FileDirectory extensionFor: filename))
		ifNotNil: [ :cls |
			cls putForm: aForm onFileNamed: filename ]
		ifNil: [
			self write: aForm onFileNamed: filename, '.bmp' ]! !

!ImageReadWriter class methodsFor: 'subclass picking' stamp: 'jmv 4/10/2015 17:06'!
subclassFor: fileExtension
	"
	ImageReadWriter subclassFor: 'bmp'
	"
	| fn |
	fn _ fileExtension asLowercase.
	self allSubclassesDo: [ :each |
		(each typicalFileExtensions includes: fn) ifTrue: [
			^ each ]].
	^ nil! !

!methodRemoval: ImageReadWriter class #canHandle:!
ImageReadWriter class removeSelector: #canHandle:!

!ImageReadWriter class reorganize!
('instance creation' onBinaryStream:)
('image reading/writing' allTypicalFileExtensions bytesFor: formFromFileNamed: formFromStream: putForm:onFileNamed: putForm:onStream: typicalFileExtensions understandsImageFormat: write:onFileNamed:)
('subclass picking' subclassFor:)
!

