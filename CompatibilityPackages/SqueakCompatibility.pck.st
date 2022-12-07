'From Cuis 6.0 [latest update: #5553] on 15 November 2022 at 11:20:39 am'!
'Description Various code used in Squeak which is not in Cuis.'!
!provides: 'SqueakCompatibility' 1 37!
!requires: 'Cuis-Base' 42 2658 nil!
SystemOrganization addCategory: 'SqueakCompatibility'!
SystemOrganization addCategory: 'SqueakCompatibility-System-Files'!


!classDefinition: #KlattResonatorIndices category: 'SqueakCompatibility'!
SharedPool subclass: #KlattResonatorIndices
	instanceVariableNames: ''
	classVariableNames: 'R1c R1vp R2c R2fp R2vp R3c R3fp R3vp R4c R4fp R4vp R5c R5fp R6c R6fp R7c R8c Rnpc Rnpp Rnz Rout Rtpc Rtpp Rtz'
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'KlattResonatorIndices class' category: 'SqueakCompatibility'!
KlattResonatorIndices class
	instanceVariableNames: ''!

!classDefinition: #StringHolder category: 'SqueakCompatibility'!
TextModel subclass: #StringHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'StringHolder class' category: 'SqueakCompatibility'!
StringHolder class
	instanceVariableNames: ''!

!classDefinition: #Model category: 'SqueakCompatibility'!
ActiveModel subclass: #Model
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'Model class' category: 'SqueakCompatibility'!
Model class
	instanceVariableNames: ''!

!classDefinition: #TimeStamp category: 'SqueakCompatibility'!
DateAndTime subclass: #TimeStamp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TimeStamp class' category: 'SqueakCompatibility'!
TimeStamp class
	instanceVariableNames: ''!

!classDefinition: #ByteString category: 'SqueakCompatibility'!
String variableByteSubclass: #ByteString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ByteString class' category: 'SqueakCompatibility'!
ByteString class
	instanceVariableNames: ''!

!classDefinition: #MultiByteFileStream category: 'SqueakCompatibility'!
UniFileStream subclass: #MultiByteFileStream
	instanceVariableNames: 'lineEndConvention'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'MultiByteFileStream class' category: 'SqueakCompatibility'!
MultiByteFileStream class
	instanceVariableNames: ''!

!classDefinition: #CrLfFileStream category: 'SqueakCompatibility'!
MultiByteFileStream subclass: #CrLfFileStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'CrLfFileStream class' category: 'SqueakCompatibility'!
CrLfFileStream class
	instanceVariableNames: ''!

!classDefinition: #FileStream category: 'SqueakCompatibility-System-Files'!
ReadWriteStream subclass: #FileStream
	instanceVariableNames: 'rwmode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility-System-Files'!
!classDefinition: 'FileStream class' category: 'SqueakCompatibility-System-Files'!
FileStream class
	instanceVariableNames: ''!

!classDefinition: #StandardFileStream category: 'SqueakCompatibility-System-Files'!
FileStream subclass: #StandardFileStream
	instanceVariableNames: 'name fileID buffer1'
	classVariableNames: 'Registry'
	poolDictionaries: ''
	category: 'SqueakCompatibility-System-Files'!
!classDefinition: 'StandardFileStream class' category: 'SqueakCompatibility-System-Files'!
StandardFileStream class
	instanceVariableNames: ''!

!classDefinition: #InvalidDirectoryError category: 'SqueakCompatibility'!
Error subclass: #InvalidDirectoryError
	instanceVariableNames: 'pathName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'InvalidDirectoryError class' category: 'SqueakCompatibility'!
InvalidDirectoryError class
	instanceVariableNames: ''!

!classDefinition: #ProvideAnswerNotification category: 'SqueakCompatibility'!
Notification subclass: #ProvideAnswerNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ProvideAnswerNotification class' category: 'SqueakCompatibility'!
ProvideAnswerNotification class
	instanceVariableNames: ''!

!classDefinition: #ClassTestCase category: 'SqueakCompatibility'!
TestCase subclass: #ClassTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ClassTestCase class' category: 'SqueakCompatibility'!
ClassTestCase class
	instanceVariableNames: ''!

!classDefinition: #TestObjectsAsMethods category: 'SqueakCompatibility'!
TestCase subclass: #TestObjectsAsMethods
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TestObjectsAsMethods class' category: 'SqueakCompatibility'!
TestObjectsAsMethods class
	instanceVariableNames: ''!

!classDefinition: #FileDirectory category: 'SqueakCompatibility'!
Object subclass: #FileDirectory
	instanceVariableNames: 'pathName'
	classVariableNames: 'CurrentDirectory DirectoryClass ImageDirectory VMDirectory'
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'FileDirectory class' category: 'SqueakCompatibility'!
FileDirectory class
	instanceVariableNames: ''!

!classDefinition: #AcornFileDirectory category: 'SqueakCompatibility'!
FileDirectory subclass: #AcornFileDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'AcornFileDirectory class' category: 'SqueakCompatibility'!
AcornFileDirectory class
	instanceVariableNames: ''!

!classDefinition: #DosFileDirectory category: 'SqueakCompatibility'!
FileDirectory subclass: #DosFileDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'DosFileDirectory class' category: 'SqueakCompatibility'!
DosFileDirectory class
	instanceVariableNames: ''!

!classDefinition: #UnixFileDirectory category: 'SqueakCompatibility'!
FileDirectory subclass: #UnixFileDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'UnixFileDirectory class' category: 'SqueakCompatibility'!
UnixFileDirectory class
	instanceVariableNames: ''!

!classDefinition: #SmalltalkImage category: 'SqueakCompatibility'!
Object subclass: #SmalltalkImage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'SmalltalkImage class' category: 'SqueakCompatibility'!
SmalltalkImage class
	instanceVariableNames: ''!

!classDefinition: #SqDirectoryEntry category: 'SqueakCompatibility'!
Object subclass: #SqDirectoryEntry
	instanceVariableNames: 'directory name creationTime modificationTime fileSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'SqDirectoryEntry class' category: 'SqueakCompatibility'!
SqDirectoryEntry class
	instanceVariableNames: ''!

!classDefinition: #DirectoryEntryDirectory category: 'SqueakCompatibility'!
SqDirectoryEntry subclass: #DirectoryEntryDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'DirectoryEntryDirectory class' category: 'SqueakCompatibility'!
DirectoryEntryDirectory class
	instanceVariableNames: ''!

!classDefinition: #DirectoryEntryFile category: 'SqueakCompatibility'!
SqDirectoryEntry subclass: #DirectoryEntryFile
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'DirectoryEntryFile class' category: 'SqueakCompatibility'!
DirectoryEntryFile class
	instanceVariableNames: ''!

!classDefinition: #UnsupportedInCuis category: 'SqueakCompatibility'!
Object subclass: #UnsupportedInCuis
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'UnsupportedInCuis class' category: 'SqueakCompatibility'!
UnsupportedInCuis class
	instanceVariableNames: ''!

!classDefinition: #AlignmentMorph category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #AlignmentMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'AlignmentMorph class' category: 'SqueakCompatibility'!
AlignmentMorph class
	instanceVariableNames: ''!

!classDefinition: #BorderStyle category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #BorderStyle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'BorderStyle class' category: 'SqueakCompatibility'!
BorderStyle class
	instanceVariableNames: ''!

!classDefinition: #ComponentInstance category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #ComponentInstance
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ComponentInstance class' category: 'SqueakCompatibility'!
ComponentInstance class
	instanceVariableNames: ''!

!classDefinition: #LayoutFrame category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #LayoutFrame
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'LayoutFrame class' category: 'SqueakCompatibility'!
LayoutFrame class
	instanceVariableNames: ''!

!classDefinition: #MCWorkingCopy category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #MCWorkingCopy
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'MCWorkingCopy class' category: 'SqueakCompatibility'!
MCWorkingCopy class
	instanceVariableNames: ''!

!classDefinition: #PackageOrganizer category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #PackageOrganizer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'PackageOrganizer class' category: 'SqueakCompatibility'!
PackageOrganizer class
	instanceVariableNames: ''!

!classDefinition: #PluggableTextMorph category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #PluggableTextMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'PluggableTextMorph class' category: 'SqueakCompatibility'!
PluggableTextMorph class
	instanceVariableNames: ''!

!classDefinition: #ProportionalLayout category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #ProportionalLayout
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'ProportionalLayout class' category: 'SqueakCompatibility'!
ProportionalLayout class
	instanceVariableNames: ''!

!classDefinition: #SimpleButtonMorph category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #SimpleButtonMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'SimpleButtonMorph class' category: 'SqueakCompatibility'!
SimpleButtonMorph class
	instanceVariableNames: ''!

!classDefinition: #TableLayout category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #TableLayout
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TableLayout class' category: 'SqueakCompatibility'!
TableLayout class
	instanceVariableNames: ''!

!classDefinition: #TextMorph category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #TextMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TextMorph class' category: 'SqueakCompatibility'!
TextMorph class
	instanceVariableNames: ''!

!classDefinition: #TextStyle category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #TextStyle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TextStyle class' category: 'SqueakCompatibility'!
TextStyle class
	instanceVariableNames: ''!

!classDefinition: #TranscriptStream category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #TranscriptStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TranscriptStream class' category: 'SqueakCompatibility'!
TranscriptStream class
	instanceVariableNames: ''!

!classDefinition: #TransferMorph category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #TransferMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'TransferMorph class' category: 'SqueakCompatibility'!
TransferMorph class
	instanceVariableNames: ''!

!classDefinition: #UIManager category: 'SqueakCompatibility'!
UnsupportedInCuis subclass: #UIManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SqueakCompatibility'!
!classDefinition: 'UIManager class' category: 'SqueakCompatibility'!
UIManager class
	instanceVariableNames: ''!


!Model commentStamp: '<historical>' prior: 0!
Compatibility. Prefer ActiveModel.!

!MultiByteFileStream commentStamp: 'jmv 5/8/2015 15:03' prior: 0!
Not really a MultiByteXXX. Accept #lineEndConvention and honor it, on Write. On Read, do no conversions.!

!CrLfFileStream commentStamp: '<historical>' prior: 0!
Do line ending conversion on write. By default, write files with host platform convention.!

!FileStream commentStamp: '<historical>' prior: 0!
I represent a Stream that accesses a FilePage from a File. One use for my instance is to access larger "virtual Strings" than can be stored contiguously in main memory. I restrict the objects stored and retrieved to be Integers or Characters. An end of file pointer terminates reading; it can be extended by writing past it, or the file can be explicitly truncated.
	
To use the file system for most applications, you typically create a FileStream. This is done by sending a message to a FileDirectory (file:, oldFile:, newFile:, rename:newName:) which creates a (sub)instance of me. Accesses to the file are then done via this instance.

*** On DOS, files cannot be shortened!!  ***  To overwrite a file with a shorter one, first delete the old file (FileDirectory deleteFilePath: 'Hard Disk:aFolder:dataFolder:foo') or (aFileDirectory deleteFileNamed: 'foo').  Then write your new shorter version.

Usually, the active class is StandardFileStream. But regardless of this, it is conventional to reference FileStream in code. This allows replacing StandardFileStream with something else (and not a real file in the host OS file system) if needed.!

!StandardFileStream commentStamp: 'ul 12/6/2009 05:13' prior: 0!
Provides a simple, platform-independent, interface to a file system. The instance variable rwmode, inherited from class PositionableStream, here is used to hold a Boolean -- true means opened for read-write, false means opened for read-only.  2/12/96 sw

I implement a simple read buffering scheme with the variables defined in PositionableStream (which are unused in me otherwise) in the following way:
	collection	<ByteString> or <ByteArray>	This is the buffer.
	position	<Integer>	The relative position in the buffer. Greater or equal to zero.
	readLimit	<Integer>	The number of bytes buffered. Greater or equal to zero.
Read buffering is enabled with #enableReadBuffering, disabled with #disableReadBuffering and it is enabled by default. The buffer is filled when a read attempt of an unbuffered absolute position is requested, or when a negative repositioning is made (with #position: with an argument < than the current absolute position) to an absolute position which is not buffered. In the first case, the buffer is positioned to the given absolute position. In the latter case the repositioning is made to the requested absolute position minus fourth of the buffer size. This means that further small negative repositionings won't result in buffer flushing. This is really useful when filing in code.
The read buffer is flushed (#flushReadBuffer) whenever a write attempt is made.
The buffer state is valid if and only if collection is not nil and position < readLimit.!

!FileDirectory commentStamp: '<historical>' prior: 0!
A FileDirectory represents a folder or directory in the underlying platform's file system. It carries a fully-qualified path name for the directory it represents, and can enumerate the files and directories within that directory.

A FileDirectory can be thought of as a Dictionary whose keys are the local names of files in that directory, and whose values are directory "entries". Each entry is an array of five items:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

The times are given in seconds, and can be converted to a time and date via Time>dateAndTimeFromSeconds:. See the comment in lookupEntry:... which provides primitive access to this information.
!

!DosFileDirectory commentStamp: '<historical>' prior: 0!
I represent a DOS or Windows FileDirectory.
!

!UnixFileDirectory commentStamp: '<historical>' prior: 0!
I represent a Unix FileDirectory.
!

!SqDirectoryEntry commentStamp: '<historical>' prior: 0!
an entry in a directory; a reference to either a file or a directory.!

!DirectoryEntryDirectory commentStamp: '<historical>' prior: 0!
an entry in a directory; a reference to a directory.!

!DirectoryEntryFile commentStamp: '<historical>' prior: 0!
an entry in a directory; a reference to a file.!

!KlattResonatorIndices class methodsFor: 'pool initialization' stamp: 'ar 5/18/2003 20:17'!
initialize
	"KlattResonatorIndices initialize"
	Rnpp := 1.
	Rtpp := 2.
	R1vp := 3.
	R2vp := 4.
	R3vp := 5.
	R4vp := 6.
	R2fp := 7.
	R3fp := 8.
	R4fp := 9.
	R5fp := 10.
	R6fp := 11.
	R1c := 12.
	R2c := 13.
	R3c := 14.
	R4c := 15.
	R5c := 16.
	R6c := 17.
	R7c := 18.
	R8c := 19.
	Rnpc := 20.
	Rnz := 21.
	Rtpc := 22.
	Rtz := 23.
	Rout := 24.! !

!KlattResonatorIndices class methodsFor: 'frame parameter data' stamp: 'dtl 11/15/2009 10:30'!
parameterData
	"This is a table describing the Klatt parameters. The columns are: parameter name, minimum value, maximum, parameter description, unit."
	^ #(
	"Excitation source (voice, aspiration and friction):"
		(f0 20 1000 'Fundamental frequency (hz)' hz)
		(flutter 0 1 'Amount of flutter' value)
		(jitter 0 1 'Amount of jitter' value)
		(shimmer 0 1 'Amount of shimmer' value)
		(diplophonia 0 1 'Amount of diplophonia' value)
		(voicing 0 80 'Amplitude of voicing' hz)
		(ro 0.01 1 'Relative duration of open phase of voicing waveform = Te/T0 (0.01 - 1)' value)
		(ra 0.01 0.2 'Relative duration of return phase of voicing waveform = Ta/T0 (0.01 - 1)' value)
		(rk 0.01 1 'Simmetry of the glottal pulse = (Te-Tp)/Tp (0.01 - 1)' value)
		(aspiration 0 80 'Amplitude of aspiration' dB)
		(friction 0 80 'Amplitude of friction' dB)
		(turbulence 0 80 'Amplitude of turbulence (in open glottal phase)' dB)

	"Formants frequencies and bandwidths:"	
		(f1 200 1300 'Frequency of 1st formant' hz)
		(b1 40 1000 'Bandwidth of 1st formant' hz)
		(df1 0 100 'Change in F1 during open portion of period' hz)
		(db1 0 400 'Change in B1 during open portion of period' hz)
		(f2 550 3000 'Frequency of 2nd formant' hz)
		(b2 40 1000 'Bandwidth of 2nd formant' hz)
		(f3 1200 4999 'Frequency of 3rd formant' hz)
		(b3 40 1000 'Bandwidth of 3rd formant' hz)
		(f4 1200 4999 'Frequency of 4th formant' hz)
		(b4 40 1000 'Bandwidth of 4th formant' hz)
		(f5 1200 4999 'Frequency of 5th formant' hz)
		(b5 40 1000 'Bandwidth of 5th formant' hz)
		(f6 1200 4999 'Frequency of 6th formant' hz)
		(b6 40 1000 'Bandwidth of 6th formant' hz)
		(fnp 248 528 'Frequency of nasal pole' hz)
		(bnp 40 1000 'Bandwidth of nasal pole' hz)
		(fnz 248 528 'Frequency of nasal zero' hz)
		(bnz 40 1000 'Bandwidth of nasal zero' hz)
		(ftp 300 3000 'Frequency of tracheal pole' hz)
		(btp 40 1000 'Bandwidth of tracheal pole' hz)
		(ftz 300 3000 'Frequency of tracheal zero' hz)
		(btz 40 2000 'Bandwidth of tracheal zero' hz)

	"Parallel Friction-Excited:"
		(a2f 0 80 'Amplitude of friction-excited parallel 2nd formant' dB)
		(a3f 0 80 'Amplitude of friction-excited parallel 3rd formant' dB)
		(a4f 0 80 'Amplitude of friction-excited parallel 4th formant' dB)
		(a5f 0 80 'Amplitude of friction-excited parallel 5th formant' dB)
		(a6f 0 80 'Amplitude of friction-excited parallel 6th formant' dB)
		(bypass 0 80 'Amplitude of friction-excited parallel bypass path' dB)
		(b2f 40 1000 'Bandwidth of friction-excited parallel 2nd formant' hz)
		(b3f 60 1000 'Bandwidth of friction-excited parallel 2nd formant' hz)
		(b4f 100 1000 'Bandwidth of friction-excited parallel 2nd formant' hz)
		(b5f 100 1500 'Bandwidth of friction-excited parallel 2nd formant' hz)
		(b6f 100 4000 'Bandwidth of friction-excited parallel 2nd formant' hz)

	"Parallel Voice-Excited:"
		(anv 0 80 'Amplitude of voice-excited parallel nasal formant' dB)
		(a1v 0 80 'Amplitude of voice-excited parallel 1st formant' dB)
		(a2v 0 80 'Amplitude of voice-excited parallel 2nd formant' dB)
		(a3v 0 80 'Amplitude of voice-excited parallel 3rd formant' dB)
		(a4v 0 80 'Amplitude of voice-excited parallel 4th formant' dB)
		(atv 0 80 'Amplitude of voice-excited parallel tracheal formant' dB)

	"Overall gain:"
		(gain 0 80 'Overall gain' dB))! !

!KlattResonatorIndices class methodsFor: 'frame parameter data' stamp: 'dtl 11/15/2009 10:31'!
parameterNames
	^ self parameterData collect: [ :each | each first]! !

!MultiByteFileStream methodsFor: 'access' stamp: 'jmv 5/8/2015 15:12'!
lineEndConvention: aSymbol

	lineEndConvention _ aSymbol caseOf: {
		[ #lf ] -> [ String lfString ].
		[ #cr ] -> [ String crString ] }.! !

!MultiByteFileStream methodsFor: 'line end conversion' stamp: 'jmv 5/8/2015 15:17'!
nextPut: char
	(char isLineSeparator and: [ lineEndConvention notNil ])
		ifTrue: [self nextPutAll: lineEndConvention ]
		ifFalse: [super nextPut: char ].
	^char! !

!MultiByteFileStream methodsFor: 'line end conversion' stamp: 'jmv 5/8/2015 15:17'!
nextPutAll: aString
	| converted |
	converted _ lineEndConvention
		ifNil: [ aString ]
		ifNotNil: [ aString withLineEndings: lineEndConvention ].
	.super nextPutAll: converted.
	^aString! !

!CrLfFileStream methodsFor: 'initialization' stamp: 'jmv 5/8/2015 15:28'!
initialize
	"By default, use host platform convention"
	lineEndConvention _ FileDirectory pathNameDelimiter = $\
		ifFalse: [ String lfString ]
		ifTrue: [ String crlfString ]! !

!FileStream methodsFor: 'accessing' stamp: 'ar 8/6/2001 18:34'!
contents
	"Return the contents of the receiver. Do not close or otherwise touch the receiver. Return data in whatever mode the receiver is in (e.g., binary or text)."
	| s savePos |
	savePos _ self position.
	self position: 0.
	s _ self next: self size.
	self position: savePos.
	^s! !

!FileStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
contentsOfEntireFile
	"Read all of the contents of the receiver."

	| s |
	self readOnly.
	self reset.
	s _ self next: self size.
	self close.
	^s! !

!FileStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
next
	(position >= readLimit and: [self atEnd])
		ifTrue: [^nil]
		ifFalse: [^collection at: (position _ position + 1)]! !

!FileStream methodsFor: 'accessing' stamp: 'nice 11/22/2009 18:11'!
next: anInteger

	| newCollection howManyRead increment |
	newCollection := self collectionSpecies new: anInteger.
	howManyRead := 0.
	[howManyRead < anInteger] whileTrue:
		[self atEnd ifTrue:
			[(howManyRead + 1) to: anInteger do: [:i | newCollection at: i put: (self next)].
			^newCollection].
		increment := (readLimit - position) min: (anInteger - howManyRead).
		newCollection replaceFrom: (howManyRead + 1)
			to: (howManyRead := howManyRead + increment)
			with: collection
			startingAt: (position + 1).
		position := position + increment].
	^newCollection! !

!FileStream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
nextPut: aByte
	"1/31/96 sw: subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
nextPutAll: aCollection
	"1/31/96 sw: made subclass responsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
size
	"Answer the size of the file in characters.
	 1/31/96 sw: made subclass responsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'testing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
atEnd
	"Answer true if the current position is >= the end of file position.
	 1/31/96 sw: subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:38'!
isFileStream
	^true! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
position
	"Answer the current character position in the file.
	 1/31/96 sw: subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
position: pos
	"Set the current character position in the file to pos.
	 1/31/96 sw: made subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
reset
	"Set the current character position to the beginning of the file.
	 1/31/96 sw: subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
setToEnd
	"Set the current character position to the end of the File. The same as
	self position: self size.  1/31/96 sw: made subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
skip: n
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file
	1/31/96 sw: made subclassResponsibility."
 
	self subclassResponsibility! !

!FileStream methodsFor: 'positioning' stamp: 'JMM 5/24/2001 22:58'!
truncate: pos
	"Truncate file to pos"

	self subclassResponsibility! !

!FileStream methodsFor: 'printing' stamp: 'tk 12/5/2001 09:12'!
longPrintOn: aStream
	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."
! !

!FileStream methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:34'!
longPrintOn: aStream limitedTo: sizeLimit indent: indent

	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."

	aStream newLine! !

!FileStream methodsFor: 'file open/close' stamp: 'jm 9/21/1998 13:02'!
close
	"Close this file."

	self subclassResponsibility
! !

!FileStream methodsFor: 'file open/close' stamp: 'jm 9/21/1998 13:02'!
closed
	"Answer true if this file is closed."

	self subclassResponsibility
! !

!FileStream methodsFor: 'file open/close' stamp: 'jm 9/21/1998 13:03'!
flush
	"When writing, flush the current buffer out to disk."

	self subclassResponsibility
! !

!FileStream methodsFor: 'file open/close' stamp: 'jm 9/21/1998 13:04'!
reopen
	"Ensure that the receiver is open, re-open it if necessary."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	self subclassResponsibility
! !

!FileStream methodsFor: 'file modes' stamp: 'jm 9/21/1998 12:59'!
readOnly
	"Set this file's mode to read-only."

	self subclassResponsibility
! !

!FileStream methodsFor: 'file modes' stamp: 'jm 9/21/1998 13:00'!
readWrite
	"Set this file's mode to read-write."

	self subclassResponsibility
! !

!FileStream methodsFor: 'file modes' stamp: 'jmv 12/9/2013 17:48'!
useBytes
	"Set this file to binary mode."

	self subclassResponsibility
! !

!FileStream methodsFor: 'file modes' stamp: 'jmv 12/9/2013 17:49'!
useCharacters
	"Set this file to ascii (text) mode."

	self subclassResponsibility
! !

!FileStream methodsFor: 'file accessing' stamp: 'jmv 10/16/2015 13:05'!
localName
	self subclassResponsibility! !

!FileStream methodsFor: 'file accessing' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
name
	"Answer the name of the file for the page the receiver is streaming over.  1/31/96 sw: made subclassResponsibility"

	self subclassResponsibility! !

!FileStream methodsFor: 'fileIn/Out' stamp: 'jmv 1/22/2019 12:27:38'!
fileIn
	"Guarantee that the receiver is readOnly before fileIn for efficiency and
	to eliminate remote sharing conflicts."

	self readOnly.
	self fileInAnnouncing: 'Loading ', self localName.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].
! !

!FileStream class methodsFor: 'instance creation' stamp: 'jmv 8/28/2011 21:59'!
new
	^ self basicNew initialize! !

!FileStream class methodsFor: 'concrete classes' stamp: 'Squeak1.1 9/23/1996 20:34:59'!
concreteStream
	"Who should we really direct class queries to?  9/21/96 tk"
	^ StandardFileStream! !

!StandardFileStream methodsFor: 'open/close' stamp: 'ul 12/4/2009 07:18'!
close
	"Close this file."

	fileID ifNotNil: [
		collection ifNotNil: [
			readLimit := position := 0 ].
		self primClose: fileID.
		self unregister.
		fileID := nil].
! !

!StandardFileStream methodsFor: 'open/close' stamp: 'ul 12/6/2009 02:45'!
closed
	"Answer true if this file is closed."

	^ fileID == nil or: [ (self primSizeNoError: fileID) == nil ]
! !

!StandardFileStream methodsFor: 'open/close' stamp: 'jm 9/21/1998 16:20'!
ensureOpen
	"Make sure that this file really is open."

	self closed ifTrue: [^ self reopen].
	(self primSizeNoError: fileID) ifNotNil: [^ self].
	self reopen.
! !

!StandardFileStream methodsFor: 'open/close' stamp: 'jmv 6/9/2021 12:43:48'!
open: fileName forWrite: writeMode 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID _ StandardFileStream retryWithGC: [ self primOpen: (fileName asUtf8: true) writable: writeMode ]
					until: [ :id | id notNil ]
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name _ fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode _ writeMode.
	buffer1 _ String new: 1.
	self enableReadBuffering
! !

!StandardFileStream methodsFor: 'open/close' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
openReadOnly
	"Open the receiver as a read-only file.  1/31/96 sw"

	^ self open: name forWrite: false! !

!StandardFileStream methodsFor: 'open/close' stamp: 'jmv 12/14/2013 11:02'!
reopen
	"Close and reopen this file. The file position is reset to zero."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	| binary |
	binary _ self isBinary.
	fileID ifNotNil: [
		collection ifNotNil: [
			position < readLimit ifTrue: [
				self flushReadBuffer ] ].
		self primCloseNoError: fileID ].
	self open: name forWrite: rwmode.
	binary ifTrue: [self binary]
! !

!StandardFileStream methodsFor: 'properties-setting' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isBinary
	^ buffer1 class == ByteArray! !

!StandardFileStream methodsFor: 'properties-setting' stamp: 'tk 11/4/1998 19:17'!
isReadOnly

	^ rwmode not
! !

!StandardFileStream methodsFor: 'properties-setting' stamp: 'jm 9/21/1998 13:56'!
readOnly
	"Make this file read-only."

	rwmode _ false.
! !

!StandardFileStream methodsFor: 'properties-setting' stamp: 'jm 9/21/1998 13:56'!
readWrite
	"Make this file writable."

	rwmode _ true.
! !

!StandardFileStream methodsFor: 'properties-setting' stamp: 'jmv 12/9/2013 17:48'!
useBytes
	buffer1 := ByteArray new: 1.
	collection ifNotNil: [ collection := collection asByteArray ]! !

!StandardFileStream methodsFor: 'properties-setting' stamp: 'jmv 12/9/2013 17:49'!
useCharacters
	"opposite of binary"
	buffer1 := String new: 1.
	collection ifNotNil: [ collection := collection asString ]! !

!StandardFileStream methodsFor: 'access' stamp: 'jmv 10/27/2015 17:07'!
directory
	"Return the directory containing this file."

	^ self fileEntry parent! !

!StandardFileStream methodsFor: 'access' stamp: 'jmv 10/27/2015 17:07'!
fileEntry
	^ self fullName asFileEntry! !

!StandardFileStream methodsFor: 'access' stamp: 'jm 9/21/1998 14:19'!
fullName
	"Answer this file's full path name."

	^ name
! !

!StandardFileStream methodsFor: 'access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
isDirectory
	"Answer whether the receiver represents a directory.  For the post-transition case, uncertain what to do.  2/14/96 sw"
	^ false! !

!StandardFileStream methodsFor: 'access' stamp: 'jmv 10/27/2015 17:08'!
localName
	^ self fileEntry name! !

!StandardFileStream methodsFor: 'access' stamp: 'jm 9/21/1998 14:19'!
name
	"Answer this file's full path name."

	^ name
! !

!StandardFileStream methodsFor: 'access' stamp: 'jmv 3/1/2010 13:35'!
peekFor: item 
	"Answer false and do not advance if the next element is not equal to item, or if this stream is at the end.  If the next element is equal to item, then advance over it and return true"
	| next |
	"self atEnd ifTrue: [^ false]. -- SFStream will give nil"
	(next _ self next) ifNil: [^ false].
	item = next ifTrue: [^ true].
	self skip: -1.
	^ false! !

!StandardFileStream methodsFor: 'access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
printOn: aStream
	"Put a printed version of the receiver onto aStream.  1/31/96 sw"

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name! !

!StandardFileStream methodsFor: 'access' stamp: 'ar 6/16/2002 18:58'!
reset
	self ensureOpen.
	self position: 0.! !

!StandardFileStream methodsFor: 'access' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
size
	"Answer the size of the file in characters.  2/12/96 sw"

	^ self primSize: fileID! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ul 12/3/2009 06:44'!
atEnd
	"Answer whether the receiver is at its end.  "
	
	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].
	^self primAtEnd: fileID! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/8/2017 17:02:10'!
basicNext
	"Answer the next byte or character (depending on mode) from this file, or nil if at the end of the file."
	
	| count |
	collection ifNotNil: [
		position < readLimit 
			ifFalse: [ 
				readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
				position := 0.
				readLimit = 0 ifTrue: [ ^nil ] ].
		^collection at: (position := position + 1) ].	
	count _ self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	^count = 1
		ifTrue: [ buffer1 at: 1 ]! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 9/5/2016 21:14:06'!
crc16
	"Copied from String>>crc16"
	
	| buffer crc |
	self reset.
	buffer _ String new: 2000.
	crc _ 0.
	[ self atEnd ] whileFalse: [
		buffer _ self nextInto: buffer.
		buffer do: [:c |
			crc _ (crc bitShift: -8) bitXor: (
			 #(	16r0000	16rC0C1	16rC181	16r0140	16rC301	16r03C0	16r0280	16rC241
				16rC601	16r06C0	16r0780	16rC741	16r0500	16rC5C1	16rC481	16r0440
				16rCC01	16r0CC0	16r0D80	16rCD41	16r0F00	16rCFC1	16rCE81	16r0E40
				16r0A00	16rCAC1	16rCB81	16r0B40	16rC901	16r09C0	16r0880	16rC841
				16rD801	16r18C0	16r1980	16rD941	16r1B00	16rDBC1	16rDA81	16r1A40
				16r1E00	16rDEC1	16rDF81	16r1F40	16rDD01	16r1DC0	16r1C80	16rDC41
				16r1400	16rD4C1	16rD581	16r1540	16rD701	16r17C0	16r1680	16rD641
				16rD201	16r12C0	16r1380	16rD341	16r1100	16rD1C1	16rD081	16r1040
				16rF001	16r30C0	16r3180	16rF141	16r3300	16rF3C1	16rF281	16r3240
				16r3600	16rF6C1	16rF781	16r3740	16rF501	16r35C0	16r3480	16rF441
				16r3C00	16rFCC1	16rFD81	16r3D40	16rFF01	16r3FC0	16r3E80	16rFE41
				16rFA01	16r3AC0	16r3B80	16rFB41	16r3900	16rF9C1	16rF881	16r3840
				16r2800	16rE8C1	16rE981	16r2940	16rEB01	16r2BC0	16r2A80	16rEA41
				16rEE01	16r2EC0	16r2F80	16rEF41	16r2D00	16rEDC1	16rEC81	16r2C40
				16rE401	16r24C0	16r2580	16rE541	16r2700	16rE7C1	16rE681	16r2640
				16r2200	16rE2C1	16rE381	16r2340	16rE101	16r21C0	16r2080	16rE041
				16rA001	16r60C0	16r6180	16rA141	16r6300	16rA3C1	16rA281	16r6240
				16r6600	16rA6C1	16rA781	16r6740	16rA501	16r65C0	16r6480	16rA441
				16r6C00	16rACC1	16rAD81	16r6D40	16rAF01	16r6FC0	16r6E80	16rAE41
				16rAA01	16r6AC0	16r6B80	16rAB41	16r6900	16rA9C1	16rA881	16r6840
				16r7800	16rB8C1	16rB981	16r7940	16rBB01	16r7BC0	16r7A80	16rBA41
				16rBE01	16r7EC0	16r7F80	16rBF41	16r7D00	16rBDC1	16rBC81	16r7C40
				16rB401	16r74C0	16r7580	16rB541	16r7700	16rB7C1	16rB681	16r7640
				16r7200	16rB2C1	16rB381	16r7340	16rB101	16r71C0	16r7080	16rB041
				16r5000	16r90C1	16r9181	16r5140	16r9301	16r53C0	16r5280	16r9241
				16r9601	16r56C0	16r5780	16r9741	16r5500	16r95C1	16r9481	16r5440
				16r9C01	16r5CC0	16r5D80	16r9D41	16r5F00	16r9FC1	16r9E81	16r5E40
				16r5A00	16r9AC1	16r9B81	16r5B40	16r9901	16r59C0	16r5880	16r9841
				16r8801	16r48C0	16r4980	16r8941	16r4B00	16r8BC1	16r8A81	16r4A40
				16r4E00	16r8EC1	16r8F81	16r4F40	16r8D01	16r4DC0	16r4C80	16r8C41
				16r4400	16r84C1	16r8581	16r4540	16r8701	16r47C0	16r4680	16r8641
				16r8201	16r42C0	16r4380	16r8341	16r4100	16r81C1	16r8081	16r4040)
			 at: ((crc bitXor: c numericValue) bitAnd: 16rFF) + 1) 
		].
	].
	^crc! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ls 8/23/2003 16:44'!
findString: string
	"Fast version of #upToAll: to find a String in a file starting from the beginning.
	Returns the position and also sets the position there.
	If string is not found 0 is returned and position is unchanged."

	| pos buffer count oldPos sz |
	oldPos _ self position.
	self reset.
	sz _ self size.
	pos _ 0.
	buffer _ String new: 2000.
	[ buffer := self nextInto: buffer.
	(count _ buffer findString: string) > 0
		ifTrue: ["Found the string part way into buffer"
			self position: pos.
			self next: count - 1.
			^self position ].
	pos _ ((pos + 2000 - string size) min: sz).
	self position: pos.
	pos = sz] whileFalse.
	"Never found it, and hit end of file"
	self position: oldPos.
	^0! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'gk 10/2/2003 09:47'!
findStringFromEnd: string
	"Fast version to find a String in a file starting from the end.
	Returns the position and also sets the position there.
	If string is not found 0 is returned and position is unchanged."

	| pos buffer count oldPos |
	oldPos _ self position.
	self setToEnd.
	pos _ self position.
	[ pos _ ((pos - 2000 + string size) max: 0).  "the [+ string size] allows for the case where the end of the search string is at the beginning of the current buffer"
	self position: pos.
	buffer _ self next: 2000.
	(count _ buffer findString: string) > 0
		ifTrue: ["Found the string part way into buffer"
			self position: pos.
			self next: count-1.  "use next instead of position:, so that CrLfFileStream can do its magic if it is being used"
			^self position].
	pos = 0] whileFalse.
	"Never found it, and hit beginning of file"
	self position: oldPos.
	^0! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ar 2/6/2001 17:59'!
flush
	"Flush pending changes"
	^self primFlush: fileID! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'mir 2/25/2000 12:37'!
next
	"Answer the next byte from this file, or nil if at the end of the file."

	^ self basicNext! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ul 12/5/2009 07:19'!
next: n
	"Return a string with the next n characters of the filestream in it.  1/31/96 sw"
	^ self nextInto: (self collectionSpecies new: n)! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ul 12/6/2009 04:14'!
next: n into: aString startingAt: startIndex
	"Read n bytes into the given string.
	Return aString or a partial copy if less than
	n elements have been read."
	
	| count  newN newStartIndex |
	collection 
		ifNil: [ 
			newN := n.
			newStartIndex := startIndex ]
		ifNotNil: [
			aString class isBytes 
				ifFalse: [ 
					position < readLimit ifTrue: [ self flushReadBuffer ].
					newN := n.
					newStartIndex := startIndex ]
				ifTrue: [
					| available |
					(available := readLimit - position) > 0 
						ifFalse: [ available := 0 ]
						ifTrue: [
							| bufferedCount |
							bufferedCount := n min: available.
							aString
								replaceFrom: startIndex
								to: startIndex + bufferedCount - 1
								with: collection
								startingAt: position + 1.
							position := position + bufferedCount.
							bufferedCount = n ifTrue: [ ^aString ] ].
					newN := n - available.
					newStartIndex := startIndex + available ] ].
	count := self primRead: fileID into: aString
				startingAt: newStartIndex count: newN.
	count = newN
		ifTrue:[ ^aString ]
		ifFalse:[ ^aString copyFrom: 1 to: newStartIndex + count - 1 ]! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ul 12/6/2009 04:11'!
next: anInteger putAll: aString startingAt: startIndex
	"Store the next anInteger elements from the given collection."
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [
		position < readLimit ifTrue: [ self flushReadBuffer ] ].	
	self primWrite: fileID from: aString startingAt: startIndex count: anInteger.
	^aString! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/8/2017 17:02:58'!
nextPut: char
	"Write the given byte or character (depending on mode) to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	buffer1 at: 1 put: char.
	self primWrite: fileID from: buffer1 startingAt: 1 count: 1.
	^ char
! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ul 12/6/2009 04:11'!
nextPutAll: aString
	"Write all the characters of the given string to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: aString startingAt: 1 count: aString basicSize.
	^ aString
! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'tk 2/5/2000 21:58'!
nextWordsInto: aBitmap
	"Note: The file primitives automatically adjust for word based objects."

	self next: aBitmap basicSize into: aBitmap startingAt: 1.
	aBitmap restoreEndianness.
	^ aBitmap! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/8/2017 17:58:13'!
padToEndIfCantTruncate
	"Only makes sense for file streams with existing content.
	On file systems that don't support truncating this is needed.
	If truncating is supported, try that first"

	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	self truncate.
	self atEnd ifTrue: [^ self].
	pad := self isBinary 
		ifTrue: [Character space numericValue]
		ifFalse: [Character space ].
	self nextPutAll: (self collectionSpecies new: ((self size - self position) min: 20000) 
							withAll: pad)! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 11/17/2016 09:07:20'!
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	position < readLimit ifTrue: [
		^collection at: position+1 ].
	self atEnd ifTrue: [^ nil].
	next _ self basicNext.
	self position: self position - 1.
	^ next! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'tk 10/19/2001 11:29'!
peekLast
	"Return that item just put at the end of the stream"

	^ buffer1 size > 0 
		ifTrue: [buffer1 last]
		ifFalse: [nil]
! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ul 12/4/2009 06:59'!
position
	"Return the receiver's current file position.  2/12/96 sw"

	collection ifNotNil: [
		position < readLimit ifTrue: [
			^(self primGetPosition: fileID) - readLimit + position ] ].
	^self primGetPosition: fileID! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ul 12/6/2009 04:08'!
position: pos
	"Set the receiver's position as indicated.  2/12/96 sw"

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| newPosition |
			newPosition := pos - (self primGetPosition: fileID) + readLimit.
			newPosition < 0 ifTrue: [
					| offsetPos |
					self primSetPosition: fileID to: (offsetPos := pos - (collection size // 4) max: 0).
					readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
					position := pos - offsetPos.
					^self ].
			newPosition < readLimit 
				ifTrue: [
					position := newPosition.
					^self ]
				ifFalse: [
					readLimit := position := 0 ] ] ].
	^self primSetPosition: fileID to: pos! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'nice 12/7/2009 08:10'!
readInto: byteArray startingAt: startIndex count: count
	"Read into the given array as specified, and return the count
	actually transferred.  index and count are in units of bytes or
	longs depending on whether the array is Bitmap, String or ByteArray"
	
	^(self next: count into: byteArray startingAt: startIndex) size - startIndex + 1
! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
setToEnd
	"Set the position of the receiver to the end of file.  1/31/96 sw"

	self position: self size! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ul 12/8/2009 00:13'!
skip: n
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file.  1/31/96 sw"

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| newPosition |
			((newPosition := position + n) >= 0 and: [ newPosition < readLimit ])
				ifTrue: [ 
					position := newPosition.
					^self ] ] ].
	self position: self position + n! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'JMM 5/24/2001 22:00'!
truncate
	"Truncate to zero"

	^ self truncate: 0! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'JMM 5/24/2001 22:47'!
truncate: pos
	"Truncate to this position"

	self position: pos.
	^self primTruncate: fileID to: pos! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/25/2021 10:00:18'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
	"See comment at PositionableStream.
	Fast version to speed up nextChunk"

	| pos buffer count skipSeparator tryAgain |
	skipSeparator _ delimiterIsTerminator ifTrue: [0] ifFalse: [1].
	collection ifNotNil: [
		(position < readLimit and: [
			(pos _ collection indexOf: delimiter startingAt: position + 1) <= readLimit and: [
				pos > 0 ] ]) ifTrue: [
					^ collection copyFrom: position + 1 to: (position _ pos) - skipSeparator ] ].

	pos _ self position.
	buffer _ self next: 2000.
	(count _ buffer indexOf: delimiter) > 0 ifTrue: [
		"Found the delimiter part way into buffer"
		self position: pos + count.
		^ buffer copyFrom: 1 to: count - skipSeparator].

	self atEnd ifTrue: [
		"Never found it, and hit end of file"
		^ delimiterIsTerminator ifTrue: [self position: pos. nil] ifFalse: [buffer]].

	"Never found it, but there's more..."
	tryAgain _ self upTo: delimiter delimiterIsTerminator: delimiterIsTerminator.
	tryAgain ifNil: [
		self position: pos.
		^ nil ].
	^ buffer, tryAgain.! !

!StandardFileStream methodsFor: 'read, write, position' stamp: 'ul 12/3/2009 06:42'!
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	^self collectionSpecies streamContents: [ :newStream |
		| next |
		[ (next := self next) == nil ] whileFalse: [
			newStream nextPut: next ] ]! !

!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primAtEnd: id
	"Answer true if the file position is at the end of the file."

	<primitive: 'primitiveFileAtEnd' module: 'FilePlugin'>
	self primitiveFailed
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primClose: id
	"Close this file."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	self primitiveFailed
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primCloseNoError: id
	"Close this file. Don't raise an error if the primitive fails."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/6/2001 17:58'!
primFlush: id
	"Flush pending changes to the disk"
	| p |
	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
	"In some OS's seeking to 0 and back will do a flush"
	p _ self position.
	self position: 0; position: p! !

!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primGetPosition: id
	"Get this files current position."

	<primitive: 'primitiveFileGetPosition' module: 'FilePlugin'>
	self primitiveFailed
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'jmv 6/9/2021 12:42:59'!
primOpen: fileNameAsUTF8 writable: writableFlag
	"Open a file of the given name, and return the file ID obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	^ nil
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primRead: id into: byteArray startingAt: startIndex count: count
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self closed ifTrue: [^ self error: 'File is closed'].
	self error: 'File read failed'.
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSetPosition: id to: anInteger
	"Set this file to the given position."

	<primitive: 'primitiveFileSetPosition' module: 'FilePlugin'>
	self primitiveFailed
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSize: id
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSizeNoError: id
	"Answer the size of this file. Answer nil if the primitive fails; this indicates that the file handle has become stale."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	^ nil
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'JMM 5/24/2001 21:55'!
primTruncate: id to: anInteger
	"Truncate this file to the given position."

	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed
! !

!StandardFileStream methodsFor: 'primitives' stamp: 'jmv 4/7/2013 17:34'!
primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: name)
		signal: (self closed
			ifTrue: [ 'File [', name, '] is closed' ]
			ifFalse: [ 'File [', name, '] write failed' ])! !

!StandardFileStream methodsFor: 'registry' stamp: 'ar 3/21/98 17:23'!
register
	^self class register: self! !

!StandardFileStream methodsFor: 'registry' stamp: 'ar 3/21/98 17:23'!
unregister
	^self class unregister: self! !

!StandardFileStream methodsFor: 'finalization' stamp: 'ar 3/21/98 18:16'!
actAsExecutor
	super actAsExecutor.
	name := nil.! !

!StandardFileStream methodsFor: 'finalization' stamp: 'ar 10/7/1998 15:44'!
finalize
	self primCloseNoError: fileID.! !

!StandardFileStream methodsFor: 'private' stamp: 'jmv 10/8/2017 17:04:44'!
collectionSpecies
	"Answer the species of collection into which the receiver can stream.
	This is ByteArray or String, depending on the mode."
	
	^buffer1 species! !

!StandardFileStream methodsFor: 'private' stamp: 'ul 12/6/2009 04:12'!
disableReadBuffering

	collection ifNotNil: [
		position < readLimit
			ifTrue: [
				| currentPosition |
				currentPosition := self position.
				collection := readLimit := position := nil.
				self position: currentPosition ]
			ifFalse: [
				collection := readLimit := position := nil ] ]
		! !

!StandardFileStream methodsFor: 'private' stamp: 'ul 12/6/2009 02:49'!
enableReadBuffering

	collection ifNil: [
		buffer1 ifNotNil: [
			collection := self collectionSpecies new: 2048 ] ].
	readLimit := position := 0! !

!StandardFileStream methodsFor: 'private' stamp: 'ul 12/6/2009 04:11'!
flushReadBuffer

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| currentPosition |
			currentPosition := self position.
			position := readLimit := 0.
			self primSetPosition: fileID to: currentPosition ] ]! !

!StandardFileStream methodsFor: 'printing' stamp: 'jmv 12/15/2021 16:00:31'!
storeOn: aStream
	"A file can't be serialized"

	^ self printOn: aStream! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'pb 5/25/2016 01:34'!
crc16OfFileNamed: fileName
	"
	StandardFileStream crc16OfFileNamed: 'cursor.jpeg'
	StandardFileStream crc16OfFileNamed: 'deafultPID.txt'
	"

	^[fileName asFileEntry readStreamDo: [ :stream |
		stream crc16 ]]
			on: FileDoesNotExistException do: nil! !

!StandardFileStream class methodsFor: 'registry' stamp: 'ul 8/27/2010 23:24'!
register: anObject
	
	^self registry add: anObject! !

!StandardFileStream class methodsFor: 'registry' stamp: 'ul 8/27/2010 23:24'!
registry
	
	^Registry ifNil: [ Registry := WeakRegistry new ]! !

!StandardFileStream class methodsFor: 'registry' stamp: 'jmv 4/25/2016 15:14'!
retryWithGC: execBlock until: testBlock forFileNamed: fullName
	"Re-implemented to only force GC if a file with the given name exists"
	| blockValue foundIt |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	"See if we have a file with the given name"
	foundIt _ self registry keys "hold on strongly for now" 
		anySatisfy:[:file| file name sameAs: fullName].
	foundIt ifFalse:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.! !

!StandardFileStream class methodsFor: 'registry' stamp: 'ul 8/27/2010 23:25'!
unregister: anObject
	
	^self registry remove: anObject ifAbsent: nil! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'len 5/20/2020 08:30:22'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection _ (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue:
		[ newName _ self request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFileEntry ].
	self halt! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'len 5/20/2020 08:30:28'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName entry |
	entry _ fullFileName asFileEntry.
	dir _ entry parent.
	localName _ entry name.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ self request: 'Enter a new file name' initialAnswer: fullFileName.
		^ FileIOAccessor default privateNewFile: newName asFileEntry ].

	self error: 'Please close this to abort file opening'! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'len 5/20/2020 08:31:13'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ fullFileName asFileEntry parent.
	files _ dir fileNames.
	fileName _ fullFileName asFileEntry name.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName _ (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ self request: 'Enter a new file name' initialAnswer: fileName.
		"If Cancel was pressed, no file should be opened - Hernan"
		newName isEmpty ifTrue: [ ^nil ]].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !

!StandardFileStream class methodsFor: 'cached state access' stamp: 'jmv 6/17/2015 16:43'!
releaseClassCachedState

	Registry ifNotNil: [
		"Make the best effort to empty the registry.
		Only keep files that are still open have references from some object."
		Registry isEmpty ifFalse: [
			Smalltalk garbageCollectMost.
			Registry isEmpty ifFalse: [
				Smalltalk garbageCollect.
				Registry isEmpty ifFalse: [
					Registry finalizeValues ]]].

		"No open files. Can nil the Registry."
		Registry isEmpty ifTrue: [
			Registry _ nil ]
	]! !

!InvalidDirectoryError methodsFor: 'exceptionDescription' stamp: 'ar 5/30/2001 20:49'!
defaultAction
	"Return an empty list as the default action of signaling the occurance of an invalid directory."
	^#()! !

!InvalidDirectoryError methodsFor: 'accessing' stamp: 'ar 5/30/2001 20:44'!
pathName
	^pathName! !

!InvalidDirectoryError methodsFor: 'accessing' stamp: 'ar 5/30/2001 20:45'!
pathName: badPathName
	pathName _ badPathName! !

!InvalidDirectoryError class methodsFor: 'exceptionInstantiator' stamp: 'ar 5/30/2001 20:49'!
pathName: badPathName
	^self new pathName: badPathName! !

!FileDirectory methodsFor: 'printing' stamp: 'jm 12/4/97 19:41'!
printOn: aStream 
	"Refer to the comment in Object|printOn:."

	aStream nextPutAll: self class name.
	aStream nextPutAll: ' on '.
	pathName printOn: aStream.
! !

!FileDirectory methodsFor: 'comparing' stamp: 'jmv 1/11/2015 21:03'!
= aDirectory

	"Any object is equal to itself"
	self == aDirectory ifTrue: [ ^ true ].
	(aDirectory is: #FileDirectory) ifFalse: [ ^false ].

	"Compare two FileDirectory instances."
	^ (pathName asString 
			compare: aDirectory pathName asString 
			caseSensitive: (self isCaseSensitive | aDirectory isCaseSensitive)) = 2! !

!FileDirectory methodsFor: 'comparing' stamp: 'cwp 11/16/2009 22:10'!
hash
	"Hash is reimplemented because #= is reimplemented"
	^pathName asString asLowercase hash! !

!FileDirectory methodsFor: 'file directory' stamp: 'bf 9/5/2010 00:02'!
assureExistence
	"Make sure the current directory exists. If necessary, create all parts in between"

	self exists ifFalse: [
 		self containingDirectory
			assureExistence;
			createDirectory: self localName]! !

!FileDirectory methodsFor: 'file directory' stamp: 'RAA 7/28/2000 13:47'!
localNameFor: fullName
	"Return the local part the given name."

	^self class localNameFor: fullName! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jm 5/8/1998 20:48'!
checkName: aFileName fixErrors: fixing
	"Check a string aFileName for validity as a file name. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is just to truncate the name to the maximum length for this platform. Subclasses can do any kind of checking and correction appropriate for their platform."

	| maxLength |
	aFileName size = 0 ifTrue: [self error: 'zero length file name'].
	maxLength _ self class maxFileNameLength.
	aFileName size > maxLength ifTrue: [
		fixing
			ifTrue: [^ aFileName contractTo: maxLength]
			ifFalse: [self error: 'file name is too long']].

	^ aFileName
! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 6/17/2015 11:23'!
fileNamesMatching: pat
	"FileDirectory smalltalkImageDirectory fileNamesMatching: '*'"

	^ self fileNames select: [:name | pat match: name]
! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 4/19/2015 08:35'!
fullNameFor: fileName
	"Return a corrected, fully-qualified name for the given file name. If the given name is already a full path (i.e., it contains a delimiter character), assume it is already a fully-qualified name. Otherwise, prefix it with the path to this directory. In either case, correct the local part of the file name."
	"Details: Note that path relative to a directory, such as '../../foo' are disallowed by this algorithm.  Also note that this method is tolerent of a nil argument -- is simply returns nil in this case."

	| correctedLocalName prefix |
	fileName ifNil: [^ nil].
	self class activeDirectoryClass splitName: fileName to:
		[:filePath :localName |
			correctedLocalName _ localName isEmpty 
				ifFalse: [self checkName: localName fixErrors: true]
				ifTrue: [localName].
			prefix _ self fullPathFor: filePath].
	prefix isEmpty
		ifTrue: [^correctedLocalName].
	prefix last = self pathNameDelimiter
		ifTrue:[^ prefix, correctedLocalName]
		ifFalse:[^ prefix, self slash, correctedLocalName]! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jm 12/4/97 21:19'!
isLegalFileName: aString 
	"Answer true if the given string is a legal file name."

	^ (self checkName: aString fixErrors: true) = aString
! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 12/6/2011 13:59'!
lastNameFor: baseFileName extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name"

	| files splits |

	files _ self fileNamesMatching: (baseFileName,'*', self class dot, extension).
	splits _ files collect: [:file | self splitNameVersionExtensionFor: file].
	splits _ splits asArray sort: [:a :b | (a at: 2) < (b at: 2)].
	^splits isEmpty 
			ifTrue: [nil]
			ifFalse: [((splits last at: 1), '.', (splits last at: 2) asString, self class dot, extension) asFileName]! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 12/6/2011 11:01'!
nextNameFor: baseFileName coda: fileNameCoda extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	"

	| files splits version |

	files _ self fileNamesMatching: (baseFileName,'*', self class dot, extension).
	splits _ files collect: [:file | self splitNameVersionExtensionFor: file].
	splits _ splits asArray sort: [:a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [version _ 1]
			ifFalse: [version _ (splits last at: 2) + 1].
	^ (baseFileName, fileNameCoda, '.', version asString, self class dot, extension) asFileName! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'jmv 12/6/2011 10:39'!
nextNameFor: baseFileName extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name"

	^self nextNameFor: baseFileName coda: '' extension: extension! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'nk 12/13/2002 10:07'!
relativeNameFor: aFileName
	"Return the full name for aFileName, assuming that aFileName is a name relative to me."
	aFileName isEmpty ifTrue: [ ^pathName ].
	^aFileName first = self pathNameDelimiter
		ifTrue: [ pathName, aFileName ]
		ifFalse: [ pathName, self slash, aFileName ]
! !

!FileDirectory methodsFor: 'file name utilities' stamp: 'djp 10/27/1999 08:58'!
splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in nextSequentialNameFor: for more details"

	| baseName version extension i j |

	baseName _ self class baseNameFor: fileName.
	extension _ self class extensionFor: fileName.
	i _ j _ baseName findLast: [:c | c isDigit not].
	i = 0
		ifTrue: [version _ 0]
		ifFalse:
			[(baseName at: i) = $.
				ifTrue:
					[version _ (baseName copyFrom: i+1 to: baseName size) asNumber.
					j _ j - 1]
				ifFalse: [version _ 0].
			baseName _ baseName copyFrom: 1 to: j].
	^ Array with: baseName with: version with: extension! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jm 1/5/98 20:49'!
containingDirectory
	"Return the directory containing this directory."

	^ FileDirectory on: (FileDirectory dirPathFor: pathName)
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'nk 2/23/2001 11:35'!
directoryEntry
	^self containingDirectory entryAt: self localName! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 4/19/2015 08:34'!
directoryEntryFor: filenameOrPath
	"Answer the directory entry for the given file or path. Sorta like a poor man's stat()."
	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:[:filePath :name |
		fName _ name.
		filePath isEmpty
			ifTrue: [dir _ self]
			ifFalse: [dir _ FileDirectory on: filePath]].
	self isCaseSensitive 
		ifTrue:[^dir entries detect:[:entry| entry name = fName] ifNone: nil]
		ifFalse:[^dir entries detect:[:entry| entry name sameAs: fName] ifNone: nil]! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jm 12/5/97 15:46'!
directoryNamed: localFileName
	"Return the subdirectory of this directory with the given name."

	^ FileDirectory on: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:21'!
directoryNames
	"Return a collection of names for the subdirectories of this directory."
	"FileDirectory smalltalkImageDirectory directoryNames"
	^ (self entries select: [ :entry |
		entry isDirectory ]) collect: [ :entry |
		entry name ]! !

!FileDirectory methodsFor: 'enumeration' stamp: 'dtl 1/14/2018 15:26:38'!
entries
	"Return a collection of directory entries for the files and directories in the directory with the given path. See primLookupEntryIn:index: for further details."
	"
	FileDirectory smalltalkImageDirectory entries
	"

	| entries index done entryArray |
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	[ done ] whileFalse: [
		entryArray _ self primLookupEntryIn: pathName index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ (InvalidDirectoryError pathName: pathName) signal ].
		entryArray
			ifNil: [ done _ true ]
			ifNotNil: [ entries
				addLast: ( (entryArray at: 4)
					ifTrue: [DirectoryEntry pathName: self pathName, self slash, entryArray first]
					ifFalse: [FileEntry pathName: self pathName, self slash, entryArray first] ) ].
		index _ index + 1 ].

	^ entries asArray! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:21'!
fileAndDirectoryNames
	"FileDirectory smalltalkImageDirectory fileAndDirectoryNames"

	^ self entries collect: [:entry | entry name]
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 11:23'!
fileNames
	"Return a collection of names for the files (but not directories) in this directory."
	"FileDirectory smalltalkImageDirectory fileNames"
	^ (self entries select: [ :entry |
		entry isDirectory not ]) collect: [ :entry |
		entry name ]! !

!FileDirectory methodsFor: 'enumeration' stamp: 'ar 3/15/2001 23:20'!
fullName
	"Return the full name of this directory."

	^pathName! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jm 9/27/1998 21:34'!
fullNamesOfAllFilesInSubtree
	"Answer a collection containing the full names of all the files in the subtree of the file system whose root is this directory."

	| result todo dir |
	result _ OrderedCollection new: 100.
	todo _ OrderedCollection with: self.
	[todo size > 0] whileTrue: [
		dir _ todo removeFirst.
		dir fileNames do: [:n | result add: (dir fullNameFor: n)].
		dir directoryNames do: [:n | todo add: (dir directoryNamed: n)]].
	^ result asArray
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'jm 12/5/97 15:39'!
keysDo: nameBlock
	"Evaluate the given block for each file or directory name in this directory."

	^ self fileAndDirectoryNames do: nameBlock
! !

!FileDirectory methodsFor: 'enumeration' stamp: 'ar 2/6/2001 15:48'!
localName
	"Return the local name of this directory."

	^FileDirectory localNameFor: pathName! !

!FileDirectory methodsFor: 'file operations' stamp: 'MPH 10/15/2000 12:43'!
copyFile: fileStream1 toFile: fileStream2
	| buffer |
	buffer _ String new: 50000.
	[fileStream1 atEnd] whileFalse:
		[fileStream2 nextPutAll: (fileStream1 nextInto: buffer)].
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 6/17/2015 11:21'!
copyFileNamed: fileName1 toFileNamed: fileName2
	"Copy the contents of the existing file with the first name into a new file with the second name. Both files are assumed to be in this directory."
	"FileDirectory smalltalkImageDirectory copyFileNamed: 'todo.txt' toFileNamed: 'todocopy.txt'"

	| file1 file2 |
	file1 _ (self readOnlyFileNamed: fileName1) binary.
	file2 _ (self newFileNamed: fileName2) binary.
	self copyFile: file1 toFile: file2.
	file1 close.
	file2 close.
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 6/17/2015 11:21'!
copyFileWithoutOverwriteConfirmationNamed: fileName1 toFileNamed: fileName2
	"Copy the contents of the existing file with the first name into a file with the second name (which may or may not exist). If the second file exists, force an overwrite without confirming.  Both files are assumed to be in this directory."
	"FileDirectory smalltalkImageDirectory copyFileWithoutOverwriteConfirmationNamed: 'todo.txt' toFileNamed: 'todocopy.txt'"

	| file1 file2 |
	fileName1 = fileName2 ifTrue: [^ self].
	file1 _ (self readOnlyFileNamed: fileName1) binary.
	file2 _ (self forceNewFileNamed: fileName2) binary.
	self copyFile: file1 toFile: file2.
	file1 close.
	file2 close.! !

!FileDirectory methodsFor: 'file operations' stamp: 'jm 12/4/97 22:55'!
createDirectory: localFileName
	"Create a directory with the given name in this directory. Fail if the name is bad or if a file or directory with that name already exists."

 	self primCreateDirectory: (self fullNameFor: localFileName).
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jm 4/9/1999 18:02'!
deleteDirectory: localDirName
	"Delete the directory with the given name in this directory. Fail if the path is bad or if a directory by that name does not exist."

 	self primDeleteDirectory: (self fullNameFor: localDirName).
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 3/2/2010 09:54'!
deleteFileNamed: localFileName
	"Delete the file with the given name in this directory."

	self deleteFileNamed: localFileName ifAbsent: nil! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 3/1/2010 11:22'!
deleteFileNamed: localFileName ifAbsent: failBlock
	"Delete the file of the given name if it exists, else evaluate failBlock.
	If the first deletion attempt fails do a GC to force finalization of any lost references. ar 3/21/98 17:53"
	| fullName |
	fullName _ self fullNameFor: localFileName.
	(UniFileStream 
		retryWithGC:[self primDeleteFileNamed: (self fullNameFor: localFileName)]
		until:[:result| result notNil]
		forFileNamed: fullName) ifNil: [
			^failBlock value].
! !

!FileDirectory methodsFor: 'file operations' stamp: 'tpr 3/26/2002 16:48'!
deleteLocalFiles
	"Delete the local files in this directory."

	self fileNames do:[:fn| self deleteFileNamed: fn ifAbsent: [(CannotDeleteFileException new
			messageText: 'Could not delete the old version of file ' , (self fullNameFor: fn)) signal]]
! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 6/17/2015 11:24'!
fileOrDirectoryExists: filenameOrPath
	"Answer true if either a file or a directory file of the given name exists. The given name may be either a full path name or a local name within this directory."
	"FileDirectory smalltalkImageDirectory fileOrDirectoryExists: Smalltalk defaultSourcesName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to: [ :filePath :name |
		fName _ name.
		dir _ filePath isEmpty
			ifTrue: [self]
			ifFalse: [FileDirectory on: filePath]].

	^ (dir includesKey: fName) or: [ fName = '' and:[ dir entries size > 1]]! !

!FileDirectory methodsFor: 'file operations' stamp: 'MPH 10/23/2000 13:31'!
putFile: file1 named: destinationFileName
	"Copy the contents of the existing fileStream into the file destinationFileName in this directory.  fileStream can be anywhere in the fileSystem."

	| file2 |
	file1 binary.
	(file2 _ self newFileNamed: destinationFileName) ifNil: [^ false].
	file2 binary.
	self copyFile: file1 toFile: file2.
	file1 close.
	file2 close.
	^ true
! !

!FileDirectory methodsFor: 'file operations' stamp: 'tk 2/26/2000 12:54'!
putFile: file1 named: destinationFileName retry: aBool
	"Copy the contents of the existing fileStream into the file destinationFileName in this directory.  fileStream can be anywhere in the fileSystem.  No retrying for local file systems."

	^ self putFile: file1 named: destinationFileName
! !

!FileDirectory methodsFor: 'file operations' stamp: 'tpr 3/26/2002 18:09'!
recursiveDelete
	"Delete the this directory, recursing down its tree."
	self directoryNames
		do: [:dn | (self directoryNamed: dn) recursiveDelete].
	self deleteLocalFiles.
	"should really be some exception handling for directory deletion, but no 
	support for it yet"
	self containingDirectory deleteDirectory: self localName! !

!FileDirectory methodsFor: 'file operations' stamp: 'jmv 3/2/2010 17:02'!
rename: oldFileName toBe: newFileName
	| selection oldName newName |
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name."
	"Modified for retry after GC ar 3/21/98 18:09"
	oldName _ self fullNameFor: oldFileName.
	newName _ self fullNameFor: newFileName.
	(UniFileStream 
		retryWithGC:[self primRename: oldName to: newName]
		until:[:result| result notNil]
		forFileNamed: oldName) ifNotNil: [ ^self].
	(self fileExists: oldFileName) ifFalse: [
		^self error:'Attempt to rename a non-existent file'.
	].
	(self fileExists: newFileName) ifTrue:[
		selection _ (PopUpMenu labels:
'delete old version
cancel')
				startUpWithCaption: 'Trying to rename a file to be
', newFileName , '
and it already exists.'.
		selection = 1 ifTrue: [
			self deleteFileNamed: newFileName.
			^ self rename: oldFileName toBe: newFileName]].
	^self error:'Failed to rename file'.! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 6/17/2015 11:21'!
directoryExists: filenameOrPath
	"Answer true if a directory of the given name exists. The given name may be either a full path name or a local directory within this directory."
	"FileDirectory smalltalkImageDirectory directoryExists: FileDirectory smalltalkImageDirectory pathName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:
		[:filePath :name |
			fName _ name.
			filePath isEmpty
				ifTrue: [dir _ self]
				ifFalse: [dir _ self directoryNamed: filePath]].

	^dir exists and: [
		self isCaseSensitive 
			ifTrue:[dir directoryNames includes: fName]
			ifFalse:[dir directoryNames anySatisfy: [:name| name sameAs: fName]]].
! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 8/28/2011 16:13'!
exists
"Answer whether the directory exists"

	| result |
	result _ self primLookupEntryIn: pathName index: 1.
	^ result ~~ #badDirectoryPath! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 6/17/2015 11:23'!
fileExists: filenameOrPath
	"Answer true if a file of the given name exists. The given name may be either a full path name or a local file within this directory."
	"FileDirectory smalltalkImageDirectory fileExists: Smalltalk defaultSourcesName"

	| fName dir |
	self class activeDirectoryClass splitName: filenameOrPath to:
		[:filePath :name |
			fName _ name.
			filePath isEmpty
				ifTrue: [dir _ self]
				ifFalse: [dir _ FileDirectory on: filePath]].
	self isCaseSensitive 
		ifTrue:[^dir fileNames includes: fName]
		ifFalse:[^dir fileNames anySatisfy: [:name| name sameAs: fName]].	! !

!FileDirectory methodsFor: 'testing' stamp: 'di 11/21/1999 20:17'!
includesKey: localName
	"Answer true if this directory includes a file or directory of the given name. Note that the name should be a local file name, in contrast with fileExists:, which takes either local or full-qualified file names."
	"(FileDirectory on: Smalltalk vmPath) includesKey: 'SqueakV2.sources'"
	self isCaseSensitive
		ifTrue:[^ self fileAndDirectoryNames includes: localName]
		ifFalse:[^ self fileAndDirectoryNames anySatisfy: [:str| str sameAs: localName]].! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 9/2/2013 10:41'!
is: aSymbol
	^ aSymbol == #FileDirectory or: [ super is: aSymbol ]! !

!FileDirectory methodsFor: 'testing' stamp: 'ar 5/30/2001 21:42'!
isAFileNamed: fName
	^FileStream isAFileNamed: (self fullNameFor: fName)! !

!FileDirectory methodsFor: 'testing' stamp: 'jmv 3/16/2010 19:23'!
isCaseSensitive
	"Return true if file names are treated case sensitive"
	"Warning: This method can't always be trusted, see comment in other implementors"
	^self class isCaseSensitive! !

!FileDirectory methodsFor: 'file status' stamp: 'mdr 1/14/2000 21:16'!
entryAt: fileName  
	"find the entry with local name fileName"

	^self entryAt: fileName ifAbsent: [ self error: 'file not in directory: ', fileName ].! !

!FileDirectory methodsFor: 'file status' stamp: 'jmv 6/11/2012 12:24'!
entryAt: fileName ifAbsent: aBlock
	"Find the entry with local name fileName and answer it.
	If not found, answer the result of evaluating aBlock."
	| comparisonBlock |
	comparisonBlock _ self isCaseSensitive
		ifTrue: [
			[ :entry |
			entry name = fileName ]]
		ifFalse: [
			[ :entry |
			entry name sameAs: fileName ]].
	^ self entries
		detect: comparisonBlock
		ifNone: aBlock! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:34'!
fileNamed: localFileName
	"Open the file with the given name in this directory for reading and/or writing.
	Create it if it doesn't exist."

	^ FileStream fileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:18'!
fileNamed: localFileName do: aBlock
	"Open the file with the given name in this directory for reading and/or writing.
	Create it if it doesn't exist.
	Evaluate aBlock, and close the file"

	(self fileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
forceNewFileNamed: localFileName
	"Open the file with the given name in this directory for writing.
	If it already exists, delete it first without asking."

	^ FileStream forceNewFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:05'!
forceNewFileNamed: localFileName do: aBlock
	"Open the file with the given name in this directory for writing.
	If it already exists, delete it first without asking.
	Evaluate aBlock, and close the file"

	(self forceNewFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
newFileNamed: localFileName
	"Create a new file with the given name in this directory.
	If the file already exists, give the chance to pick another name or overwrite it."

	^ FileStream newFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 5/20/2015 08:52'!
newFileNamed: localFileName do: aBlock
	"Create a new file with the given name in this directory and pass it as argument to aBlock."

	^ FileStream newFileNamed: (self fullNameFor: localFileName) do: aBlock
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
oldFileNamed: localFileName
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, give the chance to create the file, use another name, or abort."

	^ FileStream oldFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:07'!
oldFileNamed: localFileName do: aBlock
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, give the chance to create the file, use another name, or abort.
	Evaluate aBlock, and close the file"

	(self oldFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:14'!
oldFileNamed: localFileName ifExistsDo: aBlock
	"Open the existing file with the given name in this directory.
	If the file doesn't exist, do nothing.
	If the file exists, evaluate aBlock, and close the file"

	(self oldFileOrNoneNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/17/2015 14:32'!
oldFileOrNoneNamed: fileName
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."
	| fullName |

	"If full path name is not specified, get it assuming current directory."
	fullName _ self fullNameFor: fileName.

	^ FileStream oldFileOrNoneNamed: fullName
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 7/29/2013 23:35'!
readOnlyFileNamed: localFileName
	"Open the existing file with the given name in this directory for read-only access.
	If the file doesn't exist, give the chance to pick another, use another name, or abort."

	^ FileStream readOnlyFileNamed: (self fullNameFor: localFileName)
! !

!FileDirectory methodsFor: 'file stream creation' stamp: 'jmv 6/6/2012 13:11'!
readOnlyFileNamed: localFileName do: aBlock
	"Open the existing file with the given name in this directory for read-only access.
	If the file doesn't exist, give the chance to pick another, use another name, or abort.
	Evaluate aBlock, and close the file"

	(self readOnlyFileNamed: localFileName) ifNotNil: [ :fileStream |
		[ aBlock value: fileStream ] ensure: [ fileStream close ]]! !

!FileDirectory methodsFor: 'searching' stamp: 'jmv 6/17/2015 11:24'!
filesContaining: searchString caseSensitive: aBoolean
	| aList |
	"Search the contents of all files in the receiver and its subdirectories for the search string.  Return a list of paths found.  Make the search case sensitive if aBoolean is true."

	aList _ OrderedCollection new.
	self withAllFilesDo: [ :stream |
			(stream contentsOfEntireFile includesSubstring: searchString caseSensitive: aBoolean)
				ifTrue:	[ aList add: stream name ]]
		andDirectoriesDo: [ :d | d pathName ].
	^ aList

"FileDirectory smalltalkImageDirectory filesContaining: 'includesSubstring:'  caseSensitive: true"! !

!FileDirectory methodsFor: 'searching' stamp: 'jmv 12/6/2009 22:48'!
withAllFilesDo: fileStreamBlock andDirectoriesDo: directoryBlock

	"For the receiver and all it's subdirectories evaluate directoryBlock.
	For a read only file stream on each file within the receiver 
	and it's subdirectories evaluate fileStreamBlock."

	| todo dir |

	todo := OrderedCollection with: self.
	[todo size > 0] whileTrue: [
		dir := todo removeFirst.
		directoryBlock value: dir.
		dir fileNames do: [: n |
			"For example, files inside a Mac .app file are not accessible, and the stream is nil"
			(FileStream oldFileOrNoneNamed: (dir fullNameFor: n))
				ifNotNil: [ :fileStream | fileStreamBlock value: fileStream ]].
		dir directoryNames do: [: n | 
			todo add: (dir directoryNamed: n)]]

! !

!FileDirectory methodsFor: 'path access' stamp: 'ar 12/18/1999 01:01'!
fullPathFor: path
	^path isEmpty ifTrue:[pathName] ifFalse:[path]! !

!FileDirectory methodsFor: 'path access' stamp: 'tk 5/18/1998 22:29'!
on: fullPath
	"Return another instance"

	^ self class on: fullPath! !

!FileDirectory methodsFor: 'path access' stamp: 'jm 12/5/97 12:18'!
pathName
	"Return the path from the root of the file system to this directory."

	^ pathName
! !

!FileDirectory methodsFor: 'path access' stamp: 'jm 12/5/97 12:19'!
pathNameDelimiter
	"Return the delimiter character for this kind of directory. This depends on the current platform."

	^ self class pathNameDelimiter
! !

!FileDirectory methodsFor: 'path access' stamp: 'jm 12/5/97 12:17'!
pathParts
	"Return the path from the root of the file system to this directory as an array of directory names."

	^ pathName findTokens: self pathNameDelimiter asString
! !

!FileDirectory methodsFor: 'path access' stamp: 'ar 12/18/1999 00:36'!
slash
	^self class slash! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primCreateDirectory: fullPath
	"Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primDeleteDirectory: fullPath
	"Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primDeleteFileNamed: aFileName
	"Delete the file of the given name. Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil
! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primLookupEntryIn: fullPath index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	The empty string enumerates the top-level files or drives. (For example, on Unix, the empty path enumerates the contents of '/'. On Macs and PCs, it enumerates the mounted volumes/drives.)

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad."

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath

! !

!FileDirectory methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primRename: oldFileFullName to: newFileFullName 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.
	Changed to return nil instead of failing ar 3/21/98 18:04"

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil! !

!FileDirectory methodsFor: 'private' stamp: 'jm 12/4/97 22:44'!
setPathName: pathString

	pathName _ pathString.
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 6/17/2015 11:24'!
baseNameFor: filename
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FileDirectory baseNameFor: ((FileDirectory smalltalkImageDirectory directoryNamed: 'foo.bar') fullNameFor:'blim.blam') 
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FileDirectory baseNameFor: ((FileDirectory smalltalkImageDirectory directoryNamed: 'foo.bar') fullNameFor:'blim')
		should be the same and NOT  'foo'
		
		Oh, and
		FileDirectory baseNameFor: 'foo.bar'
		should be 'foo' not '/foo' "

	| ext |
	ext _ self extensionFor: filename.
	ext isEmpty ifTrue: [
		^filename ].
	^filename copyFrom: 1 to: filename size - ext size - 1.
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'TPR 5/10/1998 21:32'!
changeSuffix
"if 'changes' is not suitable, override this message to return something that is ok"
	^'changes'! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 6/17/2015 13:43'!
checkName: fileName fixErrors: flag
	"Check a string fileName for validity as a file name on the current default file system. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is to truncate the name to 31 chars. Subclasses can do any kind of checking and correction appropriate to the underlying platform."

"This doesn't really belong in the class. Different file systems (mounted on the same machine)
can have different ideas about this. In any case, maybe this dies when we adopt FileMan."

	^ self smalltalkImageDirectory
		checkName: fileName
		fixErrors: flag
! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:35'!
dirPathFor: fullName 
	"Return the directory part the given name."
	self activeDirectoryClass
		splitName: fullName
		to: [:dirPath :localName | ^ dirPath]! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:35'!
extensionFor: pathName
	"Return the extension of given file name, if any.
	FileDirectory extensionFor: 'writings.txt'
	FileDirectory extensionFor: 'optionalstuff.pck.st'
	FileDirectory extensionFor: 'optionalstuff.pck'
	FileDirectory extensionFor: 'code.cs.st'
	FileDirectory extensionFor: 'code.cs'
	"

	| delim i |
	self splitName: pathName to: [ :path :filename |
		delim _ self activeDirectoryClass extensionDelimiter.
		{ 'cs.st' . 'pck.st' } do: [ :specialExtension |
			(filename endsWith: delim asString, specialExtension)
				ifTrue: [ ^specialExtension ]].
		i _ filename findLast: [ :c | c = delim ].
		^ i = 0
			ifTrue: [ '']
			ifFalse: [ filename copyFrom: i + 1 to: filename size ]]! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'mir 10/11/2000 17:38'!
fileName: fileName extension: fileExtension
	| extension |
	extension _ FileDirectory dot , fileExtension.
	^(fileName endsWith: extension)
		ifTrue: [fileName]
		ifFalse: [fileName , extension].! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'TPR 5/10/1998 21:31'!
imageSuffix
"if 'image' is not suitable, override this message to return something that is ok"
	^'image'! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:35'!
localNameFor: fullName 
	"Return the local part the given name."
	self activeDirectoryClass
		splitName: fullName
		to: [:dirPath :localName | ^ localName]! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 2/26/2010 10:56'!
sourceSuffix
"if 'sources' is not suitable, override this message to return something that is ok"
	^'sources'! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'bf 3/22/2000 18:04'!
splitName: fullName to: pathAndNameBlock
	"Take the file name and convert it to the path name of a directory and a local file name within that directory. FileName must be of the form: <dirPath><delimiter><localName>, where <dirPath><delimiter> is optional. The <dirPath> part may contain delimiters."

	| delimiter i dirName localName |
	delimiter _ self pathNameDelimiter.
	(i _ fullName findLast: [:c | c = delimiter]) = 0
		ifTrue:
			[dirName _ String new.
			localName _ fullName]
		ifFalse:
			[dirName _ fullName copyFrom: 1 to: (i - 1 max: 1).
			localName _ fullName copyFrom: i + 1 to: fullName size].

	^ pathAndNameBlock value: dirName value: localName! !

!FileDirectory class methodsFor: 'name utilities' stamp: 'jmv 4/19/2015 08:41'!
startUp

	Smalltalk openSourceFiles! !

!FileDirectory class methodsFor: 'cacher state access' stamp: 'jmv 6/17/2015 11:05'!
releaseClassCachedState

	DirectoryClass _ nil.
	ImageDirectory _ nil.
	VMDirectory _ nil.
	CurrentDirectory _ nil! !

!FileDirectory class methodsFor: 'system start up' stamp: 'jmv 2/15/2008 00:45'!
shutDown

	Smalltalk closeSourceFiles.
! !

!FileDirectory class methodsFor: 'private' stamp: 'jmv 4/19/2015 08:34'!
activeDirectoryClass
	"Return the concrete FileDirectory subclass for the platform on which we are currently running."

	DirectoryClass ifNotNil: [ ^DirectoryClass ].
	
	FileDirectory allSubclasses do: [ :cls |
		cls isActiveDirectoryClass ifTrue: [
			DirectoryClass _ cls.
			^ DirectoryClass ]].

	"no responding subclass; use FileDirectory"
	DirectoryClass _ FileDirectory.
	^DirectoryClass! !

!FileDirectory class methodsFor: 'private' stamp: 'TPR 5/10/1998 21:40'!
isActiveDirectoryClass
	"Does this class claim to be that properly active subclass of FileDirectory for this platform?
	Default test is whether the primPathNameDelimiter matches the one for this class. Other tests are possible"

	^self pathNameDelimiter = self primPathNameDelimiter
! !

!FileDirectory class methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primPathNameDelimiter
	"Return the path delimiter for the underlying platform's file system."

 	<primitive: 'primitiveDirectoryDelimitor' module: 'FilePlugin'>
	self primitiveFailed
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:09'!
currentDirectory
	"Answer the current directory.

	In Unix it is the current directory in the OS shell that started us.
	In Windows the same happens if the image file is in a subree of the Windows current directory.

	But it defaults to the directory in wich this Smalltalk image was started (or last saved) if this fails
	(this usually happens, for example, if the image is dropped on the VM in a Windows explorer).
	See #getCurrentWorkingDirectory
	"

	CurrentDirectory ifNil: [
		CurrentDirectory _ self on: (Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]) ].
	^ CurrentDirectory
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 13:44'!
forFullFileName: fullFileName

	| path |
	path _ self dirPathFor: fullFileName.
	^ self on: path
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 4/19/2015 08:37'!
on: pathString
	"Return a new file directory for the given path, of the appropriate FileDirectory subclass for the current OS platform."

	| pathName |
	"If path ends with a delimiter (: or /) then remove it"
	((pathName _ pathString) endsWith: self activeDirectoryClass pathNameDelimiter asString) ifTrue: [
		pathName _ pathName copyFrom: 1 to: pathName size - 1].
	^ self activeDirectoryClass new setPathName: pathName
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jm 12/4/97 23:29'!
root
	"Answer the root directory."

	^ self on: ''
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:17'!
smalltalkImageDirectory
	"Answer the directory on which this Smalltalk image was started (or last saved)"

	ImageDirectory ifNil: [
		ImageDirectory _ self on: Smalltalk imagePath ].
	^ ImageDirectory
! !

!FileDirectory class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:06'!
vmDirectory
	"Answer the directory containing the VM that runs us."

	VMDirectory ifNil: [
		VMDirectory _ self on: Smalltalk vmPath ].
	^ VMDirectory
! !

!FileDirectory class methodsFor: '*squeakCompatibility' stamp: 'jmv 6/17/2015 17:24'!
default
	"Old squeak protocol. Removed from Cuis to avoid false polymorphism and ambiguity
	(i.e. to make it easier to spot & change stuff)
	For Cuis code, use:
		#smalltalkImageDirectory (usually preferred for Smalltalk related files) or 
		#currentDirectory (usually preferred for other files, and especially for arguments of shell scripts)
	Answer #currentDirectory, even if Squeak answers #smalltalkImageDirectory, for consistency with
		FileStream fileNamed: 'filename.extension'
	without path, that is assumed to mean #currentDirectory."
	^ self currentDirectory! !

!FileDirectory class methodsFor: '*squeakCompatibility' stamp: 'jmv 6/17/2015 13:45'!
deleteFilePath: fileName
	"Delete the file after finding its directory"
	"este quiere un fullPath. Y el unico sender le manda un nombre simple...
	Modificarlo para que ande en ambos casos!! (incluso en freakin linux)
	directoryEntryFor: filenameOrPath
	"

	| dir fullName |
	
	fullName _ FileDirectory default fullNameFor: fileName.
	dir _ self forFullFileName: fullName.
	dir deleteFileNamed: (self localNameFor: fileName).
! !

!FileDirectory class methodsFor: '*squeakCompatibility' stamp: 'jmv 6/17/2015 13:14'!
directoryEntryFor: filenameOrPath
	^self default directoryEntryFor: filenameOrPath! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jm 3/27/98 08:17'!
dot
	"Return a one-character string containing the filename extension delimiter for this platform (i.e., the local equivalent of 'dot')"

	^ self extensionDelimiter asString
! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jm 3/27/98 06:57'!
extensionDelimiter
	"Return the character used to delimit filename extensions on this platform. Most platforms use the period (.) character."

	^ $.
! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jmv 3/16/2010 19:23'!
isCaseSensitive
	"Return true if file names are treated case sensitive"
	"Warning: This method can't always be trusted, see comment in other implementors"
	^true! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'nk 3/13/2003 10:58'!
makeAbsolute: path
	"Ensure that path looks like an absolute path"
	^path first = self pathNameDelimiter
		ifTrue: [ path ]
		ifFalse: [ self slash, path ]! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jmv 11/30/2014 16:23'!
makeRelative: path
	"Ensure that path looks like an relative path"
	^path first = self pathNameDelimiter
		ifTrue: [ path allButFirst ]
		ifFalse: [ path ]! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jm 5/8/1998 20:45'!
maxFileNameLength

	^ 31
! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'jmv 4/19/2015 08:38'!
pathNameDelimiter
	"return the active directory class's directory seperator character"
	"Warning: endless recursion if no specific #activeDirectoryClass found..."
	^ self activeDirectoryClass pathNameDelimiter! !

!FileDirectory class methodsFor: 'platform specific' stamp: 'ar 4/18/1999 18:18'!
slash
	^ self pathNameDelimiter asString! !

!FileDirectory class methodsFor: 'create/delete file' stamp: 'jmv 6/17/2015 11:58'!
lookInUsualPlaces: fileName
	"Check the current directory, the imagePath, and the vmPath (and the vmPath's owner) for this file."

	| dir |
	((dir _ FileDirectory currentDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	((dir _ FileDirectory smalltalkImageDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	((dir _ FileDirectory vmDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	((dir _ dir containingDirectory) fileExists: fileName)
		ifTrue: [^ dir fileNamed: fileName].

	^ nil! !

!AcornFileDirectory methodsFor: 'file name utilities' stamp: 'tpr 8/2/2003 19:34'!
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	| fName badChars hasBadChars |
	fName _ super checkName: aFileName fixErrors: fixing.
	badChars _ #( $# $: $< $> $| $? $* $" $%) asSet.
	hasBadChars _ fName includesAnyOf: badChars.
	(hasBadChars and:[fixing not]) ifTrue:[^self error:'Invalid file name'].
	hasBadChars ifFalse:[^ fName].
	^ fName collect:
		[:char | (badChars includes: char) 
				ifTrue:[$!!] 
				ifFalse:[char]]! !

!AcornFileDirectory methodsFor: 'file name utilities' stamp: 'ar 12/18/1999 00:47'!
fullPathFor: path
	path isEmpty ifTrue:[^pathName].
	((path includes: $$ ) or:[path includes: $:]) ifTrue:[^path].
	^pathName, self slash, path! !

!AcornFileDirectory methodsFor: 'testing' stamp: 'tpr 4/28/2004 21:54'!
directoryExists: filenameOrPath
"if the path is a root,we have to treat it carefully"
	(filenameOrPath endsWith: '$') ifTrue:[^(FileDirectory on: filenameOrPath) exists].
	^(self directoryNamed: filenameOrPath ) exists! !

!AcornFileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/17/2015 13:19'!
entries
	"Return a collection of directory entries for the files and directories in 
	the directory with the given path. See primLookupEntryIn:index: for 
	further details."
	"
	FileDirectory smalltalkImageDirectory entries
	"

	| entries extraPath |
	entries _ super entries.
	pathName isEmpty
		ifTrue: [
			"For Acorn we also make sure that at least the parent of the current dir 
			is added - sometimes this is in a filing system that has not been (or 
			cannot be) polled for disc root names"
			extraPath _  self class smalltalkImageDirectory containingDirectory.
			"Only add the extra path if we haven't already got the root of the current dir in the list"
			(entries anySatisfy: [:ent | extraPath fullName beginsWith: ent name]) 
				ifFalse: [
					entries _ entries
								copyWith: (DirectoryEntryDirectory
										directory: self
										name: extraPath fullName
										creationTime: 0
										modificationTime: 0
										fileSize: 0) ]].
	^ entries! !

!AcornFileDirectory methodsFor: 'path access' stamp: 'tpr 11/30/2003 21:42'!
pathParts
	"Return the path from the root of the file system to this directory as an 
	array of directory names.
	This version tries to cope with the RISC OS' strange filename formatting; 
	filesystem::discname/$/path/to/file
	where the $ needs to be considered part of the filingsystem-discname atom."
	| pathList |
	pathList := super pathParts.
	(pathList indexOf: '$') = 2
		ifTrue: ["if the second atom is root ($) then stick $ on the first atom 
				and drop the second. Yuck"
			^ Array
				streamContents: [:a | 
					a nextPut: (pathList at: 1), '/$'.
					3 to: pathList size do: [:i | a
								nextPut: (pathList at: i)]]].
	^ pathList! !

!AcornFileDirectory class methodsFor: 'platform specific' stamp: 'jmv 2/15/2008 00:41'!
isActiveDirectoryClass
	"Does this class claim to be that properly active subclass of FileDirectory  
	for the current platform? On Acorn, the test is whether platformName 
	is 'RiscOS' (on newer VMs) or if the primPathNameDelimiter is $. (on
	older ones), which is what we would like to use for a dirsep if only it
	would work out. See pathNameDelimiter for more woeful details - then
	just get on and enjoy Squeak"

	^ Smalltalk platformName = 'RiscOS'
		or: [self primPathNameDelimiter = $.]! !

!AcornFileDirectory class methodsFor: 'platform specific' stamp: 'tpr 8/1/2003 16:38'!
isCaseSensitive
	"Risc OS ignores the case of file names"
	^ false! !

!AcornFileDirectory class methodsFor: 'platform specific' stamp: 'TPR 7/20/1999 17:52'!
maxFileNameLength

	^ 255
! !

!AcornFileDirectory class methodsFor: 'platform specific' stamp: 'TPR 5/10/1998 21:45'!
pathNameDelimiter
"Acorn RiscOS uses a dot as the directory separator and has no real concept of filename extensions. We tried to make code handle this, but there are just too many uses of dot as a filename extension - so fake it out by pretending to use a slash. The file prims do conversions instead.
Sad, but pragmatic"
	^ $/
! !

!DosFileDirectory methodsFor: 'as yet unclassified' stamp: 'di 6/18/1998 08:57'!
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	| fName badChars hasBadChars |
	fName _ super checkName: aFileName fixErrors: fixing.
	badChars _ #( $: $< $> $| $/ $\ $? $* $") asSet.
	hasBadChars _ fName includesAnyOf: badChars.
	(hasBadChars and:[fixing not]) ifTrue:[^self error:'Invalid file name'].
	hasBadChars ifFalse:[^ fName].
	^ fName collect:
		[:char | (badChars includes: char) 
				ifTrue:[$#] 
				ifFalse:[char]]! !

!DosFileDirectory methodsFor: 'as yet unclassified' stamp: 'bf 3/21/2000 17:06'!
setPathName: pathString
	"Ensure pathString is absolute - relative directories aren't supported on all platforms."

	(pathString isEmpty
		or: [pathString first = $\
			or: [pathString size >= 2 and: [pathString second = $: and: [pathString first isLetter]]]])
				ifTrue: [^ super setPathName: pathString].

	self error: 'Fully qualified path expected'! !

!DosFileDirectory methodsFor: 'path access' stamp: 'je 11/8/2000 20:02'!
driveName

   "return a possible drive letter and colon at the start of a Path name, empty string otherwise"

   | firstTwoChars |

   ( pathName size >= 2 ) ifTrue: [
      firstTwoChars _ (pathName copyFrom: 1 to: 2).
      (self class isDrive: firstTwoChars) ifTrue: [^firstTwoChars]
   ].
   ^''! !

!DosFileDirectory methodsFor: 'path access' stamp: 'nk 7/18/2004 17:26'!
fullNameFor: fileName
	"Return a corrected, fully-qualified name for the given file name. If the given name is already a full path (i.e., it contains a delimiter character), assume it is already a fully-qualified name. Otherwise, prefix it with the path to this directory. In either case, correct the local part of the file name."
	fileName ifNil:[^fileName].
	"Check for fully qualified names"
	((fileName size >= 2 and: [fileName first isLetter and: [fileName second = $:]])
		or: [(fileName beginsWith: '\\') and: [(fileName occurrencesOf: $\) >= 2]])
			ifTrue:[^fileName].
	^super fullNameFor: fileName! !

!DosFileDirectory methodsFor: 'path access' stamp: 'je 11/8/2000 19:57'!
fullPathFor: path
	"Return the fully-qualified path name for the given file."
	path isEmpty ifTrue:[^pathName].
	(path at: 1) = $\ ifTrue:[
		(path size >= 2 and:[(path at: 2) = $\]) ifTrue:[^path]. "e.g., \\pipe\"
		^self driveName , path "e.g., \windows\"].
	(path size >= 2 and:[(path at: 2) = $: and:[path first isLetter]])
		ifTrue:[^path]. "e.g., c:"
	^pathName, self slash, path! !

!DosFileDirectory methodsFor: 'path access' stamp: 'nk 12/13/2002 10:05'!
relativeNameFor: path
	"Return the full name for path, assuming that path is a name relative to me."
	path isEmpty ifTrue:[^pathName].
	(path at: 1) = $\ ifTrue:[
		(path size >= 2 and:[(path at: 2) = $\]) ifTrue:[^super relativeNameFor: path allButFirst ]. "e.g., \\pipe\"
		^super relativeNameFor: path "e.g., \windows\"].
	(path size >= 2 and:[(path at: 2) = $: and:[path first isLetter]])
		ifTrue:[^super relativeNameFor: (path copyFrom: 3 to: path size) ]. "e.g., c:"
	^pathName, self slash, path! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'ar 5/1/1999 01:48'!
isCaseSensitive
	"Return true if file names are treated case sensitive"
	^false! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'ar 3/6/2004 03:46'!
isDrive: fullName
	"Answer whether the given full name describes a 'drive', e.g., one of the root directories of a Win32 file system. We allow two forms here - the classic one where a drive is specified by a letter followed by a colon, e.g., 'C:', 'D:' etc. and the network share form starting with double-backslashes e.g., '\\server'."
	^ (fullName size = 2 and: [fullName first isLetter and: [fullName last = $:]])
		or: [(fullName beginsWith: '\\') and: [(fullName occurrencesOf: $\) = 2]]! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'jm 5/8/1998 20:45'!
maxFileNameLength

	^ 255
! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'jm 12/4/97 22:57'!
pathNameDelimiter

	^ $\
! !

!DosFileDirectory class methodsFor: 'platform specific' stamp: 'ar 3/6/2004 04:14'!
splitName: fullName to: pathAndNameBlock
	"Take the file name and convert it to the path name of a directory and a local file name within that directory. 
	IMPORTANT NOTE: For 'drives', e.g., roots of the file system on Windows we treat the full name of that 'drive' as the local name rather than the path. This is because conceptually, all of these 'drives' hang off the virtual root of the entire Squeak file system, specified by FileDirectory root. In order to be consistent with, e.g., 

		DosFileDirectory localNameFor: 'C:\Windows' -> 'Windows'
		DosFileDirectory dirPathFor: 'C:\Windows' -> 'C:'

	we expect the following to be true:

		DosFileDirectory localNameFor: 'C:' -> 'C:'
		DosFileDirectory dirPathFor: 'C:'. -> ''
		DosFileDirectory localNameFor: '\\server' -> '\\server'.
		DosFileDirectory dirPathFor: '\\server' -> ''.

	so that in turn the following relations hold:

		| fd |
		fd := DosFileDirectory on: 'C:\Windows'.
		fd containingDirectory includes: fd localName.
		fd := DosFileDirectory on: 'C:'.
		fd containingDirectory includes: fd localName.
		fd := DosFileDirectory on: '\\server'.
		fd containingDirectory includes: fd localName.
	"
	(self isDrive: fullName)
		ifTrue: [^ pathAndNameBlock value:''  value: fullName].
	^ super splitName: fullName to: pathAndNameBlock! !

!UnixFileDirectory methodsFor: 'testing' stamp: 'sr 5/8/2000 12:58'!
directoryExists: filenameOrPath
	"Handles the special case of testing for the root dir: there isn't a
	possibility to express the root dir as full pathname like '/foo'."

	^ filenameOrPath = '/' or: [super directoryExists: filenameOrPath]! !

!UnixFileDirectory methodsFor: 'testing' stamp: 'sr 5/8/2000 13:03'!
fileOrDirectoryExists: filenameOrPath 
	"Handles the special case of testing for the root dir: there isn't a 
	possibility to express the root dir as full pathname like '/foo'."

	^ filenameOrPath = '/' or: [super fileOrDirectoryExists: filenameOrPath]! !

!UnixFileDirectory methodsFor: 'file names' stamp: 'bf 3/22/2000 18:24'!
fullPathFor: path
	"Return the fully-qualified path name for the given file."
	path isEmpty ifTrue: [^ pathName].
	path first = $/ ifTrue: [^ path].
	^ pathName = '/'			"Only root dir ends with a slash"
		ifTrue: ['/' , path]
		ifFalse: [pathName , '/' , path]! !

!UnixFileDirectory methodsFor: 'private' stamp: 'jm 12/4/97 23:43'!
setPathName: pathString
	"Unix path names start with a leading delimiter character."

	(pathString isEmpty or: [pathString first ~= self pathNameDelimiter])
		ifTrue: [pathName _ self pathNameDelimiter asString, pathString]
		ifFalse: [pathName _ pathString].
! !

!UnixFileDirectory class methodsFor: 'platform specific' stamp: 'jmv 3/16/2010 19:24'!
isCaseSensitive
	"Warning: This method can't always be trusted.
	For instance, the Mac supports both case sensitive and case insensitive volumes.
	Something better needs to be done."
	^ Smalltalk platformName ~= 'Mac OS'! !

!UnixFileDirectory class methodsFor: 'platform specific' stamp: 'yo 2/4/1999 06:40'!
maxFileNameLength

	^ 255! !

!UnixFileDirectory class methodsFor: 'platform specific' stamp: 'jm 9/17/97 15:48'!
pathNameDelimiter

	^ $/
! !

!SmalltalkImage class methodsFor: 'as yet unclassified' stamp: 'jmv 5/8/2015 09:46'!
current
	^Smalltalk! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 10/30/2009 15:50'!
baseName
	^ FileDirectory baseNameFor: self name! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 9/12/2007 17:36'!
containingDirectory
	"Answer the FileDirectory in which I reside."
	^ directory! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 8/10/2007 12:25'!
creationDateAndTime
	"The DateAndTime my entry in the file system was created."
	^DateAndTime fromSeconds: creationTime! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 2/15/2010 13:16'!
creationTime
	"The time the entry was created, as an Integer number of seconds offset from the DateAndTime epoch."
	^creationTime! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 9/21/2009 18:24'!
extension
	^ FileDirectory extensionFor: self name! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'ls 7/15/1998 21:38'!
fileSize
	"size of the entry, if it's a file"
	^fileSize! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'jmv 1/7/2014 00:46'!
fileSizeString
	"Answer my file size as an easy-to-read String."
	^ self fileSize printStringAsBytes! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'bgf 9/9/2010 07:36'!
fullName
	"The fully-qualified name.
	 Since this method falls into the equality test, make it safe when directory is nil."
	^ directory 
		ifNotNil: [ directory fullNameFor: self name ] 
		ifNil: [ self name ]! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 9/12/2007 17:25'!
isDirectory
	"whether this entry represents a directory"
	self subclassResponsibility! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 8/10/2007 12:25'!
modificationDateAndTime
	"The DateAndTime my entry in the file system was last modified."
	^ DateAndTime fromSeconds: modificationTime! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'ls 7/15/1998 21:37'!
modificationTime
	"time the entry was last modified"
	^modificationTime! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'ls 7/15/1998 21:37'!
name
	"name of the entry"
	^name! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 8/29/2007 17:44'!
printOn: aStream 
	super printOn: aStream.
	aStream
		space ;
		nextPutAll: self name! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'cmm 1/25/2011 13:16'!
size
	"For API compatibility with byte objects (for streaming api)."
	^ self fileSize! !

!SqDirectoryEntry methodsFor: 'access' stamp: 'bp 6/2/2014 10:49'!
splitNameVersionExtension
	" answer an array with the root name, version # and extension."
	^directory splitNameVersionExtensionFor: self name! !

!SqDirectoryEntry methodsFor: 'testing' stamp: 'jmv 5/27/2014 11:22'!
= aDirectoryEntry 

	"Any object is equal to itself"
	self == aDirectoryEntry ifTrue: [ ^ true ].

	"Answer whether I am equivalent in all of my file-system attributes."
	self species == aDirectoryEntry species ifFalse: [ ^ false ].

	^ self containingDirectory = aDirectoryEntry containingDirectory
		and: [ self name = aDirectoryEntry name
				and: [ self modificationTime = aDirectoryEntry modificationTime
						and: [ self fileSize = aDirectoryEntry fileSize ]]]! !

!SqDirectoryEntry methodsFor: 'testing' stamp: 'cmm 2/21/2011 21:50'!
exists
	^ (self containingDirectory
		entryAt: self name
		ifAbsent: [ nil ]) notNil! !

!SqDirectoryEntry methodsFor: 'testing' stamp: 'cmm 2/18/2011 16:04'!
hash
	"Hashing on directory + name should be sufficient."
	^ (self containingDirectory hash hashMultiply + self name hash) hashMultiply! !

!SqDirectoryEntry methodsFor: 'private-initialization' stamp: 'cmm 9/12/2007 17:20'!
setDirectory: aFileDirectoryOrServerDirectory name: name0  creationTime: creationTime0  modificationTime: modificationTime0 fileSize: fileSize0
	directory := aFileDirectoryOrServerDirectory.
	name := name0.
	creationTime := creationTime0.
	modificationTime := modificationTime0.
	fileSize := fileSize0! !

!DirectoryEntryDirectory methodsFor: 'convert' stamp: 'jmv 6/6/2012 14:23'!
asFileDirectory
	"Answer a FileDirectory representing the same directory I represent."
	^directory on: (directory fullNameFor: self name)! !

!DirectoryEntryDirectory methodsFor: 'testing' stamp: 'cmm 9/13/2007 12:24'!
isDirectory
	"whether this entry represents a directory, it does."
	^ true! !

!DirectoryEntryFile methodsFor: 'testing' stamp: 'cmm 9/13/2007 12:24'!
isDirectory
	"whether this entry represents a directory, it does not."
	^ false! !

!DirectoryEntryFile methodsFor: 'stream access' stamp: 'jmv 6/6/2012 14:21'!
readStreamDo: aBlock
	"Obtain a FileStream on my contents that can be read, but not written,
	and send it to aBlock."
	^ directory readOnlyFileNamed: self name do: aBlock! !

!ProtoObject methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:02'!
ifNotNilDo: ifNotNilBlock
	"Compatibility. Prefer #ifNotNil:"
	^self ifNotNil: ifNotNilBlock! !

!Object methodsFor: '*squeakCompatibility' stamp: 'jmv 5/14/2015 11:11'!
isCharacter
	^false! !

!Float methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:08:16'!
printOn: aStream base: base digitCount: numDigits

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base digitCount: numDigits]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0'. ^ self]
				ifFalse: [self negated absPrintOn: aStream base: base digitCount: numDigits]]! !

!Float methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:24:25'!
printOn: aStream base: base nDigits: numDigits

	self printOn: aStream base: base digitCount: numDigits! !

!Time class methodsFor: '*squeakCompatibility' stamp: 'jmv 11/9/2021 18:17:03'!
totalSeconds
	^self localSecondClock ! !

!UndefinedObject methodsFor: '*squeakCompatibility' stamp: 'jmv 3/1/2010 09:59'!
environment
	"Necessary to support disjoint class hierarchies."

	^Smalltalk		"No environments in Cuis..."! !

!Behavior methodsFor: '*squeakCompatibility' stamp: 'jmv 11/1/2011 23:15'!
environment
	"Return the environment in which the receiver is visible"
	^Smalltalk! !

!Character methodsFor: '*squeakCompatibility' stamp: 'jmv 9/6/2016 10:27:29'!
asInteger
	"Answer the value of the receiver."

	^self numericValue! !

!Character methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:26'!
asUnicode
	"Answer the unicode encoding of the receiver"
	"self leadingChar = 0 ifTrue: [^ self asInteger].
	^self encodedCharSet charsetClass convertToUnicode: self charCode"
	^self asInteger! !

!Character methodsFor: '*squeakCompatibility' stamp: 'jmv 11/27/2017 11:04:11'!
between: min and: max 
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self numericValue >= min numericValue and: [self numericValue <= max numericValue]! !

!Character methodsFor: '*squeakCompatibility' stamp: 'jmv 5/14/2015 11:12'!
isCharacter
	^true! !

!Character methodsFor: '*squeakCompatibility' stamp: 'jmv 9/6/2016 10:27:11'!
value
	^self numericValue! !

!Character class methodsFor: '*squeakCompatibility' stamp: 'jmv 9/6/2016 10:26:47'!
value: anInteger
	"Answer the Character whose value is anInteger."

	^ self numericValue: anInteger! !

!UnicodeCodePoint methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 17:42:45'!
asInteger
	^self codePoint! !

!UnicodeCodePoint methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 17:41:55'!
asUnicode
	"Answer the unicode encoding of the receiver"
	"self leadingChar = 0 ifTrue: [^ self asInteger].
	^self encodedCharSet charsetClass convertToUnicode: self charCode"
	^self asInteger! !

!UnicodeCodePoint methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 17:43:02'!
between: min and: max 
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self codePoint >= min codePoint and: [self codePoint <= max codePoint]! !

!UnicodeCodePoint methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 17:43:17'!
value
	^self numericValue! !

!UnicodeCodePoint class methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 17:44:39'!
value: anInteger
	"Answer the Character whose value is anInteger."

	^ self numericValue: anInteger! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg
	"Activate the receiver, with one or zero arguments."
	
	numArgs >= 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg cull: secondArg
	"Activate the receiver, with two or less arguments."
	
	numArgs >= 2 ifTrue: [ ^self value: firstArg value: secondArg ].	
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg cull: secondArg cull: thirdArg
	"Activate the receiver, with three or less arguments."
	
	numArgs >= 2 ifTrue: [ 
		numArgs >= 3 ifTrue: [ ^self value: firstArg value: secondArg value: thirdArg ].
		^self value: firstArg value: secondArg ].
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!BlockClosure methodsFor: '*squeakCompatibility' stamp: 'bp 11/29/2014 17:29'!
cull: firstArg cull: secondArg cull: thirdArg cull: fourthArg
	"Activate the receiver, with four or less arguments."
	
	numArgs >= 3 ifTrue: [
		numArgs >= 4 ifTrue: [
			^self value: firstArg value: secondArg value: thirdArg value: fourthArg ].
		^self value: firstArg value: secondArg value: thirdArg ].
	numArgs = 2 ifTrue: [ ^self value: firstArg value: secondArg ].	
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !

!InputSensor methodsFor: '*squeakCompatibility' stamp: 'jmv 5/8/2015 12:47'!
cursorPoint
	^self mousePoint! !

!SequenceableCollection methodsFor: '*squeakCompatibility' stamp: 'jmv 4/2/2016 23:18'!
doWithIndex: elementAndIndexBlock
	"Old style. Prefer #withIndexDo:"
	self withIndexDo: elementAndIndexBlock! !

!String methodsFor: '*squeakCompatibility' stamp: 'NM 10/17/2021 15:14:08'!
asInteger 
	"From Squeak 5.3" 	
	^self asIntegerSigned: true

	! !

!String methodsFor: '*squeakCompatibility' stamp: 'NM 10/17/2021 15:14:31'!
asIntegerSigned: signed
	"From Squeak 5.3 "
	"Return the first decimal integer I can find or nil."
	| index character size result negative |
	index := 0.
	size := self size.
	"Find the first character between $0 and $9."
	[ (index := index + 1) > size or: [ (self at: index) isDigit ] ] whileFalse.
	index > size ifTrue: [ ^nil ].
	negative := signed and: [ index > 1 and: [ (self at: index - 1) == $- ] ].
	"Parse the number."
	size - index > 15 ifTrue: [
		negative ifTrue: [ index := index - 1 ].
		^Integer readFrom: (
			ReadStream
				on: self
				from: index
				to: size) ].
	result := (self at: index) digitValue.
	[ (index := index + 1) <= size
		and: [ (character := self at: index) isDigit ] ]  whileTrue: [
		result := result * 10 + character digitValue ].
	negative ifTrue: [ ^result negated ].
	^result! !

!String methodsFor: '*squeakCompatibility' stamp: 'eem 2/3/2015 12:04'!
subStrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| char result sourceStream subString |
	#Collectn.
	"Changed 2000/04/08 For ANSI <readableString> protocol."
	(separators isString or:[separators allSatisfy: [:element | element isCharacter]]) ifFalse:
		[^ self error: 'separators must be Characters.'].
	sourceStream := ReadStream on: self.
	result := OrderedCollection new.
	subString := String new.
	[sourceStream atEnd]
		whileFalse: 
			[char := sourceStream next.
			(separators includes: char)
				ifTrue: [subString notEmpty
						ifTrue: 
							[result add: subString copy.
							subString := String new]]
				ifFalse: [subString := subString , (String with: char)]].
	subString notEmpty ifTrue: [result add: subString copy].
	^ result asArray! !

!String methodsFor: '*squeakCompatibility' stamp: 'hjh 5/9/2015 17:03'!
withCRs
	"Return a copy of the receiver in which backslash (\) characters have been replaced with carriage returns."

	^ self collect: [ :c | c = $\ ifTrue: [ Character cr ] ifFalse: [ c ]].! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
cr
	^self crString! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
crlf
	^self crlfString! !

!String class methodsFor: '*squeakCompatibility' stamp: 'jmv 12/18/2013 00:42'!
lf
	^self lfString! !

!UnicodeString methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 17:46:17'!
asInteger 
	"From Squeak 5.3" 	
	^self asIntegerSigned: true

	! !

!UnicodeString methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 17:46:22'!
asIntegerSigned: signed
	"From Squeak 5.3 "
	"Return the first decimal integer I can find or nil."
	| index character size result negative |
	index := 0.
	size := self size.
	"Find the first character between $0 and $9."
	[ (index := index + 1) > size or: [ (self at: index) isDigit ] ] whileFalse.
	index > size ifTrue: [ ^nil ].
	negative := signed and: [ index > 1 and: [ (self at: index - 1) == $- ] ].
	"Parse the number."
	size - index > 15 ifTrue: [
		negative ifTrue: [ index := index - 1 ].
		^Integer readFrom: (
			ReadStream
				on: self
				from: index
				to: size) ].
	result := (self at: index) digitValue.
	[ (index := index + 1) <= size
		and: [ (character := self at: index) isDigit ] ]  whileTrue: [
		result := result * 10 + character digitValue ].
	negative ifTrue: [ ^result negated ].
	^result! !

!UnicodeString methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 17:46:26'!
subStrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| char result sourceStream subString |
	#Collectn.
	"Changed 2000/04/08 For ANSI <readableString> protocol."
	(separators isString or:[separators allSatisfy: [:element | element isCharacter]]) ifFalse:
		[^ self error: 'separators must be Characters.'].
	sourceStream := ReadStream on: self.
	result := OrderedCollection new.
	subString := String new.
	[sourceStream atEnd]
		whileFalse: 
			[char := sourceStream next.
			(separators includes: char)
				ifTrue: [subString notEmpty
						ifTrue: 
							[result add: subString copy.
							subString := String new]]
				ifFalse: [subString := subString , (String with: char)]].
	subString notEmpty ifTrue: [result add: subString copy].
	^ result asArray! !

!UnicodeString methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 17:46:30'!
withCRs
	"Return a copy of the receiver in which backslash (\) characters have been replaced with carriage returns."

	^ self collect: [ :c | c = $\ ifTrue: [ Character cr ] ifFalse: [ c ]].! !

!UnicodeString class methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 18:06:21'!
cr
	^self crString! !

!UnicodeString class methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 18:06:25'!
crlf
	^self crlfString! !

!UnicodeString class methodsFor: '*squeakCompatibility' stamp: 'jmv 10/31/2022 18:06:29'!
lf
	^self lfString! !

!CompiledMethod methodsFor: '*squeakCompatibility' stamp: 'NS 12/12/2003 15:18'!
isAbstract
	| marker |
	marker := self markerOrNil.
	^ marker notNil and: [self class abstractMarkers includes: marker].! !

!CompiledMethod methodsFor: '*squeakCompatibility' stamp: 'al 2/13/2006 17:44'!
markerOrNil
	"If I am a marker method, answer the symbol used to mark me.  Otherwise
	answer nil.

	What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	used to indicate ('mark') a special property.

	Marker methods compile to bytecode like:

		9 <70> self
		10 <D0> send: <literal 1>
		11 <87> pop
		12 <78> returnSelf

	for the first form, or 

		9 <70> self
		10 <D0> send: <literal 1>
		11 <7C> returnTop

	for the second form."

	| e |
	((e := self endPC) = 19 or: [e = 20]) ifFalse: [^ nil].
	(self numLiterals = 3) ifFalse:[^ nil].
	(self at: 17) =  16r70 ifFalse:[^ nil].		"push self"
	(self at: 18) = 16rD0 ifFalse:[^ nil].		"send <literal 1>"
	"If we reach this point, we have a marker method that sends self <literal 1>"
	^ self literalAt: 1
! !

!CompiledMethod class methodsFor: '*squeakCompatibility' stamp: 'NS 12/12/2003 15:17'!
abstractMarkers
	^ #(subclassResponsibility shouldNotImplement)! !

!SystemDictionary methodsFor: '*squeakCompatibility' stamp: 'jmv 7/2/2019 08:08:30'!
datedVersion
	^ self version! !

!Stream methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:02:52'!
print: aNumber digits: numDigits

	aNumber printOn: self base: 10 nDigits: numDigits ! !

!PositionableStream methodsFor: '*SqueakCompatibility-nonhomogeneous accessing'!
nextNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger."
	| s |
	s := 0.
	1 to: n do: 
		[:i | s := (s bitShift: 8) bitOr: self next asInteger].
	^ s normalize! !

!WriteStream methodsFor: '*squeakCompatibility' stamp: 'jmv 5/8/2015 14:58'!
crtab
	"Append a return character, followed by anInteger tab characters, to the 
	receiver."

	self cr; tab! !

!WriteStream methodsFor: '*squeakCompatibility' stamp: 'jmv 5/8/2015 14:59'!
crtab: anInteger
	"Append a return character, followed by anInteger tab characters, to the 
	receiver."

	self cr.
	anInteger timesRepeat: [self tab]! !

!WriteStream methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 14:59:34'!
nl
	self newLine ! !

!UniFileStream methodsFor: '*squeakCompatibility' stamp: 'jmv 4/2/2016 23:38'!
lineEndConvention: aSymbol
	"ignore it"! !

!UniFileStream class methodsFor: '*squeakCompatibility' stamp: 'jmv 5/31/2016 12:38'!
forceNewFileNamed: aFilename
	^ aFilename asFileEntry forceWriteStream! !

!Encoder methodsFor: '*squeakCompatibility' stamp: 'jmv 3/1/2010 11:18'!
environment
	"Answer the environment of the current compilation context,
	 be it in a class or global (e.g. a workspace)"
	^Smalltalk "No environments in Cuis..."! !

!Transcript class methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:04:14'!
nl
	self newLine! !

!Transcript class methodsFor: '*squeakCompatibility' stamp: 'KenD 8/26/2016 15:04:19'!
print: aNumber digits: numDigits

	aNumber printOn: self base: 10 nDigits: numDigits ! !
KlattResonatorIndices initialize!
