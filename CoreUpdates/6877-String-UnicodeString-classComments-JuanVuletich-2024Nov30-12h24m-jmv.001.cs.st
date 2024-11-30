'From Cuis7.1 [latest update: #6876] on 30 November 2024 at 12:26:54 pm'!

!String commentStamp: '<historical>' prior: 0!
A String is an indexed collection of Characters. In Cuis, Characters are Unicode Code Points. In an instance of String, all the Characters must be in the first 255 CodePoints, the Latin-1 set. See also UnicodeString.

String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:

See UnicodeString.!


!UnicodeString commentStamp: '<historical>' prior: 0!
A UnicodeString is an indexed collection of Characters. In Cuis, Characters are Unicode Code Points. See also String.

Based on https://github.com/svenvc/UTF8String by Sven Van Caekenberghe. Good ideas and implementation. Thanks Sven!!

Play with the class side demo methods.!

