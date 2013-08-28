'From Cuis 4.2 of 25 July 2013 [latest update: #1808] on 28 August 2013 at 11:20:49.509115 am'!

!String commentStamp: '<historical>' prior: 0!
A String is an indexed collection of Characters. In Cuis, Characters are represented in the Latin-9 (ISO 8859-15) encoding. Each character takes 1 byte.

String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:

Cuis includes limited support for Unicode. A ByteArray can hold any Unicode string in utf-8. There is support for converting to and from instances of String. As String can not hold all Unicode code points, (just the ISO 8859-15 subset), others are converted to NCRs. See #fromUtf8:hex:trimLastNull: and #asUtf8: .!

