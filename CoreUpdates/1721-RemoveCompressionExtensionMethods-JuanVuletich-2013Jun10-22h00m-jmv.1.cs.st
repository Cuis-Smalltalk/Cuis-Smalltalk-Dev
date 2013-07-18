'From Cuis 4.1 of 12 December 2012 [latest update: #1720] on 10 June 2013 at 10:00:51.721679 pm'!
!methodRemoval: String #unzipped!
String removeSelector: #unzipped!
!methodRemoval: StandardFileStream #compressFile!
StandardFileStream removeSelector: #compressFile!
!methodRemoval: FileStream #viewGZipContents!
FileStream removeSelector: #viewGZipContents!
!methodRemoval: ReadWriteStream #asUnZippedStream!
ReadWriteStream removeSelector: #asUnZippedStream!
!methodRemoval: ReadWriteStream #asUnZippedStream:!
ReadWriteStream removeSelector: #asUnZippedStream:!
!methodRemoval: ReadWriteStream #isZipArchive!
ReadWriteStream removeSelector: #isZipArchive!
!methodRemoval: PositionableStream #asZLibReadStream!
PositionableStream removeSelector: #asZLibReadStream!
!methodRemoval: FileList #compressFile!
FileList removeSelector: #compressFile!
!methodRemoval: FileList #serviceCompressFile!
FileList removeSelector: #serviceCompressFile!
!methodRemoval: CodePackageFile class #installMonticelloPackageStream:!
CodePackageFile class removeSelector: #installMonticelloPackageStream:!
!methodRemoval: CodePackageFile class #monticelloPackageNameFrom:!
CodePackageFile class removeSelector: #monticelloPackageNameFrom:!
!methodRemoval: CodePackageFile class #serviceInstallMonticelloPackage!
CodePackageFile class removeSelector: #serviceInstallMonticelloPackage!
!methodRemoval: CodeFileBrowser class #browseMCZCode:!
CodeFileBrowser class removeSelector: #browseMCZCode:!
!methodRemoval: CodeFileBrowser class #serviceBrowseMCZCode!
CodeFileBrowser class removeSelector: #serviceBrowseMCZCode!
!methodRemoval: ChangeList class #browseMCZContents:!
ChangeList class removeSelector: #browseMCZContents:!
!methodRemoval: ChangeList class #serviceMCZContents!
ChangeList class removeSelector: #serviceMCZContents!
!methodRemoval: Bitmap #compressGZip!
Bitmap removeSelector: #compressGZip!
!methodRemoval: ArrayedCollection #writeOnGZIPByteStream:!
ArrayedCollection removeSelector: #writeOnGZIPByteStream:!
