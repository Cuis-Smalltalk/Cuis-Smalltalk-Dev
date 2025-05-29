'From Cuis7.3 [latest update: #7254] on 29 May 2025 at 11:07:00 am'!

!MessageSend commentStamp: '<historical>' prior: 0!
Instances of MessageSend encapsulate message sends to objects. Arguments can be either predefined or supplied when the message send is performed. MessageSends are used to implement the #when:send:to: event system.

Use #value to perform a message send with its predefined arguments and #valueWithArguments: if additonal arguments have to supplied.

Structure:
 receiver		Object -- object receiving the message send
 selector			Symbol -- message selector
 arguments		Array -- bound arguments!


!WeakMessageSend commentStamp: 'jmv 5/29/2025 11:06:24' prior: 0!
Instances of WeakMessageSend encapsulate message sends to objects, like MessageSend. Unlike MessageSend it is not necessarily a valid mesage.  A request to value only results in a send if in fact it is valid. 

See MessageSend comment also. WeakMessageSend is used primarily for event registration. 

Note: References receiver and arguments weakly. If receiver or arguments get collected, #value will do nothing.!

