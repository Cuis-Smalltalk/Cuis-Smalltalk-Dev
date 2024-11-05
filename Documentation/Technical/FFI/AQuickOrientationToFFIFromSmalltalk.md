# A Quick Orientation To FFI From Smalltalk
*by sqr (Andrés Valloud)*

Alas, I'm really time constrained and I still want to write something down... so, this draft is by no means complete but it will have to do for now.


## 1.  Introduction.

So you want to interact with some library written in C from Smalltalk, and you want the convenience of interfacing from the image itself so you don't have to exit the Smalltalk programming environment.  This is why virtually every foreign function interface is designed so that you write code in the image.

Now, why the name... this is a function interface, and it so happens that the function is foreign because it doesn't live in the environment you're programming in.  So much for that.  Just say FFI for short.

The thing is, however, that whoever put your Smalltalk virtual machine or runtime environment together likely didn't use Smalltalk.  In the vast majority of cases, something like a C compiler or other C libraries will be involved.  So, first observation:

    When you use an FFI, you become a virtual machine engineer.

Probably you didn't want to sign up for that, but it's what happens anyway.  This matters because of the primary directive:

    You want your software to work.

This is easy to see: nobody uses broken software, and nobody pays for broken software in the long run.  The software may be slow, or quirky, or something else, and this is perfectly fine.  But if the stuff blows up, the whole thing is pointless.

So, this matters because getting virtual machines or other runtime environments to work is hard.  This means that using an FFI pushes you into difficult stuff.  A lot of the difficulty is due to the implied complexity.  Many wish there was something like "everyday C", where the behavior is well defined and there are no booby traps.  But for historical reasons this is going to be a bit difficult to achieve, and it is effectively outside your control anyway.  So, you will have to manage this complexity effectively.

In order to do that, you will have to learn the sources of complexity that come with C, how they affect the system as a whole, and what to do about things.  Very importantly, you will also have to learn how to watch out for common pitfalls and sinful temptations that will only result in endless headache and having to come up with fancy excuses why your stuff doesn't work.  Remember: nobody uses stuff that doesn't work, so you don't want to engage in futile efforts that are bound to fail.

Now, this is not a C programming guide.  Getting to write code in C well takes time, and substantial head scratching at times.  However, just for the purpose at hand, what you absolutely must know can be summarized as follows.

    You must get your head around how the C world works.

It doesn't matter that you don't like it, it doesn't matter that you think it's wrong and so forth.  It doesn't even matter that some proponents of C may agree with you on this.  The functionality is there and you want to use it, so obey the rules, and thus learn the rules.  Now, C has a large number of rules, and is also much more fragile than Smalltalk in many ways.  This means you will have to exercise much, but like much more care than you normally do in the image.  Above all, repeat with me:

    Code works only when you can prove it has to work.

That is, that some stuff appears to work on your laptop for five minutes, or a day, means nothing.  You must be able to explain why your code must work so well, that if it does not work you can file a bug report that the maintainer acknowledges as valid.  And in order to do that, you will have to learn where to look to come up with your explanations, as well as the proper protocol for filing a bug report in the C world.

So, this is an attempt to get you going with these matters.  If you pick up the material here, you will be far better off when you interact with the C world, you will be much less likely to experience heisenbugs, avoid wasting endless amounts of time fighting defects that will not go away, and you and your customers will have a much better experience overall.  So, let's get started.


## 2.  Encapsulation.

Everybody loves encapsulation in Smalltalk.  No object can manipulate the instance variables of some other object, everything happens with messages, and things are great.  It is obvious that this must be so, because lack of encapsulation leads to code that is unmanageable.

Well, yes, except Smalltalk provides a number of ways to do such manipulation because the system itself must be capable of doing that.  How do you think the debugger works?  And how about persistent object storage, or serialization, or changing the shape of a class by adding an instance variable?

The thing is, in Smalltalk doing such things has a psychological barrier that is very easy to see.  If you send messages like #become:, #oneWayBecome:, or #instVarAt:put:, you better know what you're doing.  If you use the mirror protocol to make such changes, you really should know what you're doing.  This is why most code doesn't use such messages: they call attention to themselves, and before long you learn to avoid them when possible and question their usage when you see them.

In C, however, there is absolutely no difference between valid code and wildly incorrect hacks.  Pointer usage just looks like

    *this = that;

regardless of whether it's even valid.  This is why it is very, very important to really understand what C code is doing, and to obey the documentation and standards to the letter: the behavior contrast caused by insignificant code differences is very, very stark.

This doesn't mean that C programmers forget good programming practices like encapsulation.  In fact, they don't: this is why you can have C programs as complicated as the Linux kernel basically ruling the world.  So, let's skip the programming language wars --- they don't help getting your code to work.

In the C world, encapsulation looks very different from Smalltalk.  First, because in C you can have multiple copies of the same code loaded and actively running (try that with your image).  Second, because the compilation process of C is also different from that of Smalltalk.

However, just because it's different or because you can reach into memory with pointers does not mean you should violate how C encapsulation works.  In fact, in C, it is even more important that you follow the rules, because any tiny slip up and you're in trouble.

In C, "trouble" is spelled out in documentation (note that in the C world, "documentation" is absolutely not "comments").  In documentation, every term is defined, and the term for trouble is used uniformly.  Here it is:

    undefined behavior.

Any time you see "undefined behavior", that means "all warranties are void, good luck to you".  Most often that means a crash.  But it can also mean silent data corruption, security issues, who knows.  That's the problem: you want defined behavior instead, because it is only with defined behavior that you can build good explanations about how your code has to work.  Code that works was the idea, right?

In particular, any encapsulation violation in C immediately gets you undefined behavior.  So, that means you must respect encapsulation at all costs.

Encapsulation in C is achieved in a number of ways.  One is that, unlike in most Smalltalks where if you try to load two versions of the class Array then the whole system will likely crash, in C you have mechanisms like versioning which means that different C libraries can request different versions of things like malloc().  For example, your nice virtual machine may need a version of glibc (GNU's C standard library, or gnu libc, or glibc) that is different from the version required by the library you want to use.  This is no problem and is handled by the C runtime.

However, it gets tricky: what happens when, at the request of the image code, the virtual machine calls malloc(), and then the image passes the resulting pointer to another library that then calls free() on that pointer?  Well, since the versions of malloc() and free() were potentially different, malloc() can be allocating with one memory manager and free() trying to release memory in another memory manager.  The specification for free() says that if you call free() with an argument that was not returned by (the corresponding) malloc(), the behavior is undefined.  So, this is bad.

Immediately this tells you that you better watch out for inadvertent crossings across different library versions, because otherwise all sorts of horrible things can happen.  Also, this is an example of encapsulation violation.  See how easy that was to do?  Now consider the fact that no C compiler would have even warned you about doing that: there was also zero resistance to you seriously compromising your program.  At best you got a memory leak because free() refused to release what it didn't allocate.  Until by accident you do pass free() a pointer that its malloc() returned, that by accident matched what the other malloc() returned, and now you have some serious corruption going on.  Also, good luck debugging that.

And there's more.  The first stage of compilation in a C program invokes a macro preprocessor.  This allows you to do text substitution before the program is ever compiled.  So, for example, if you see something like this,

    #define free(x) do { \
        printf("lala\n"); \
        free(x); \
    } while(0)

then every subsequent call to the function free() will be replaced by calling printf(), then calling free().  This is a key mechanism that allows C programs to encapsulate the details of how something that is called "free()" can be implemented: you can add code before or after, have calls to free() do something entirely different, and even do nothing at all.  For example, you could have something like this.

    #define free(x) __never_call_this_ever_free__(x)

Because the C macro preprocessor runs before C compilation begins proper, this means you cannot call macros from an FFI.  This applies to functions, data structures, and just about anything else because the C macro system is Turing complete.

So, the short story is that if you see a macro, you have to treat it like a black box, just like the code that runs when you send a message: you can invoke it, but you can't assume its implementation.  Here, you can invoke a macro only at a certain stage of compilation, and beyond that you should not depend on its implementation.  Chances are someone put the macro there so the implementation can change over time, or even change depending on when you load a program.

Sometimes you can see the macros, and sometimes you cannot.  Some compilers substitute function calls on their own, macros or not, because the manuals allow them to.  You should also note that even runtime linkers, the programs that link a library to your running program at runtime, can also support linker macros.  So, even linking functions into your programs so they can be called is subject to macros.  You are not supposed to know what these macros do, and cannot depend on them, because otherwise you're doing the equivalent of #instVarAt:.  In the C world, that's undefined behavior.  So, don't.

This seems pretty restrictive.  And it is.  This is why the use case for FFI is calling libraries that explicitly document that they provide actual functions you can call.  When that is available to you, then you can write code in Smalltalk to call such libraries without the extra cost of setting up a compilation environment, compiling primitives or plugins, and so forth.  In this sense, FFI from the image is a convenience.  For anything else, the only way to do things correctly is writing a plugin, or writing primitives, follow the documentation to the letter, and this of course means using a C compiler.

This sounds like an easy conclusion to reach, but you would be surprised.  The thing is, the philosophies in C and Smalltalk are very different.  Remember you have the advantage of seeing how (for example) encapsulation works in Smalltalk, which is very clear, so now you just have the task of recognizing what C mechanisms achieve similar goals.  But be careful: when you inadvertently allow yourself to look at C as if it was (or should be) like Smalltalk, then you silently get code that doesn't work.  So let's take a look at some examples of how things can go very wrong.


## 3.  The POSIX world.

In Linux and other Unix derived systems, the operating system functionality is provided to you in C, and this is typically documented and specified in the POSIX / Single Unix Specification standard.  You can access this standard for free by looking for the Open Group that publishes this documentation.  If you start looking at it, you will note that it's best interpreted as if you were a lawyer, or even a mathematician.  What is not guaranteed is undefined behavior by default.  All terms are defined very clearly, and you must only interpret what is said explicitly.

To be specific: if your first language is not English, you may be especially tempted to read more than what English specifications actually say.  As an example, it is very easy to start inferring attitudes and implications that would make sense for the tone of the document if it were translated directly into Spanish, but these assumptions are invalid in the culture where the English document originated from.  As a rough estimate of how serious documentation should be read, it must be as if you were listening to Mr. Spock.

Speaking of definitions, let's agree on a couple things before continuing.  If the result of doing something is undefined behavior, we will say that you can't do it.  Not because you cannot actually trash your software by doing something wrong.  But rather, because the primary directive is to write software that works.  Also, we will say that software works only when you can justify why it has to work.  The existence of any undefined behavior means code does not work, because there is no justification why it should work.  And thus, you can't do things that invoke undefined behavior.  By extension, code that merely appears to work because it hasn't crashed yet does not work.  All code is broken until proven otherwise.  It sounds draconian, but there's no real alternative.

Now, this matters because, for the purposes of doing FFI, anything that is controlled by POSIX is immediately off limits.  Why, you say?

    Because in POSIX, effectively everything can be a macro.

Struct definitions can be macros, so you don't know how big they are, so you can't properly specify them in FFI declarations.  Virtually every function can be a macro, so you can't call it from an FFI.

In POSIX, some critical structs like the ones controlling sockets even have a variable number of members, and the order is not specified.  That you know the order in one platform doesn't mean the order is the same in the other platform.  So, for this reason, anything that depends on POSIX must go through a C compiler for the target platform in question.  Otherwise, your code is not portable and so it does not work.

Similarly, some structs can have variable free space at the end for the kernel's private use.  You don't know what that size is, so you can't write it down anywhere.  This also means you can't call such things from an FFI.  Even things like the variable errno can be a macro, so you can't reference that with a pointer.

There are plenty of weird examples of this.  In some platforms, functions such as memset() do not exist.  That is, there is literally no function called memset() anywhere.  This doesn't stop you from referencing such a thing in a C program, because during compilation a macro called memset() rewrites that into what you should be using.  In Linux, the very useful function stat() can be a linker macro.  So, even if the function is there, you can't call it because any program invoking stat() will end up doing something else.

So, in the POSIX world, the only way to do correct things when calling POSIX functionality is to write plugins or primitives, and then using a C compiler.

Some argue that this is unsatisfactory and what should be done is to implement enough of a C compiler in Smalltalk so one can automatically import definitions from header files.  This might work ok for standalone libraries that are relatively clean and have few dependencies.  However, in general this approach won't work.  Remember that C macros are Turing complete, and C code routinely takes advantage of this.  For example, header files ask what compiler is running and then do one thing or another.  Suppose the header file says something like

    #ifdef __GNUC__
    ...
    #endif

and the platform compiler is GCC.  What is a Smalltalk header file parser supposed to do now?  What if the code inside the ifdef references a compiler intrinsic, something that the compiler provides and that is implementation specific and of course private?  There are a multitude of these cases.  All the effort writing effectively a C compiler in Smalltalk, just so that one can write a few lines of FFI code, can be reduced to writing a few lines of C code and using an actual C compiler from the command line.  Also, the latter is much easier to justify as correct than the former.


## 4.  Variability of the unspecified.

Ok, you must write some C code and you want to integrate it into the virtual machine via primitives or a plugin.  Of course, for best results you should use the same compilation environment that produced (say) the virtual machine you're using.

In the Unix world, this means specifying a distribution (e.g. Ubuntu), with a certain version that should be reasonably old.  This is so when you ship your binaries to people that can't be using the latest version, the binaries don't fail to load because the Linux installations don't have the latest glibc (for instance).  Also, compilation of your plugin or primitives may depend on a suite of header files that is also associated with a kernel, and so the build environment should also specify compiler version, kernel version, library versions, and so on.  Obviously, something like a virtual machine should be compiled using the official compiler for the platform, e.g. in Linux that's GCC because the kernel is compiled with GCC.  And there should be a clear procedure to construct this compilation environment from scratch (so that builds are reproducible).  Once it's set up, this compilation environment should be frozen so that you can support the binaries you ship.  This also means no random hacking of the compilation environment: absolutely minimal use of sudo, no unnecessary packages, no symlinking random files needed for compilation (if the standard environment can compile the Linux kernel and a million other packages, but it can't compile your virtual machine, guess whose fault it is --- also, you can bet it's undefined behavior), and no other modifications that are not explicitly supported in documentation.  Remember: you can't mess with a build environment.  Ideally, there should be a snapshot of a (say) Linux installation containing the official compilation environment so that everybody uses the same thing.

For the same reason, libraries and other dependencies of the binaries you compile should be arranged so tha they are linked dynamically rather than statically.  If you link them statically, the same old version from way back when will be permanently glued to your binaries, which will then carry the same bugs in said libraries forever irrespective of platform or third party upgrades.  A very visible example is how the C runtime libraries for Windows are shipped as redistributable libraries which are installed as if they were applications.  This greatly facilitates Microsoft's task of updating said libraries should there be problems.  And if programs link to said libraries dynamically, chances are that only one copy of the C runtime library will load with your program, as opposed to multiple such versions: one dynamically due to other dependencies, and several other statically linked copies coming from your binaries (note how this exacerbates the problems such as doing malloc() in one library and then free() on another).  So, as a rule of thumb, specify that libraries provided by the platform should be linked dynamically to your binaries.

Proceeding this way minimizes the variability in runtime libraries, which is a good thing.  Also, remember that sometimes different compiler versions do different things.  It would be bad if your compilation of the virtual machine with (say) a 5 line new primitive does not work at all because the virtual machine depends on a specific compiler version, and when you compile it with your compiler everything breaks regardless of your 5 line primitive.  Because guess what: now you're debugging the entire virtual machine, all because you wanted to add a tiny bit of code.  This is how resistance to change manifests in C, and this is why it's so important to always obey the documentation, respect encapsulation, and avoid undefined behavior like the pest.

Also, did you know that integer types such as short, int, long, and long long do not have specified lengths?  This is why in Windows a long is 32 bits, while a long in the POSIX world is typically 64 bits (in both cases considering 64 bit binaries).  And there are at least 5 common data models in which such integers change size.  So, if you want portable code, and the functions you want to use refer to such types, you must take these things into consideration.  Of course, portable code is better because you cut down on your maintenance headaches.  So, look for standard and portable headers and definitions, e.g. stdint.h.

Incidentally, the above means pointer types should never be confused with integer types, because they are bound to have different sizes when you least expect it.  Does your FFI support a pointer type?  Some definitely don't (and often they don't support standard integer types either --- now what?).


## 5.  Scope of applicability.

The above is also why it's critically important that something like the virtual machine itself obeys all the rules and sets the tone for the whole system.  Imagine getting random behavior you can't explain from the virtual machine: how long until you can't succeed at your project?  And if that is acceptable, then why should any code work at all?

Moreover, remember: whatever you do in FFI becomes integrated with the virtual machine.  And sometimes, you just can't do some things because they will conflict with what the virtual machine is doing.  Say you want to use some functions related to timers.  Ok, there are two such sets of functions, but you can only use one because otherwise you get undefined behavior.  That means you must check the entire virtual machine to make sure that you can use the one you want without running into conflicts.  See how you just became a virtual machine engineer?

And there are a multitude of ways in which things can go wrong.  Calling memcpy() with overlapping memory areas is undefined behavior.  So is not restoring errno in signal handlers, or calling any function in signal handlers that is not in the POSIX list of approved functions for signal handlers.  That the code might appear to work should just reinforce the notion that it is actually broken.  You can't call a function with more arguments than it takes, even if the code compiles.  You must be aware of pointer aliasing rules, and how writing code any which way will get you into trouble.  Modern compilers assume that pointers of different types point to different memory, in this way they can optimize reads and writes better.  This also means that code like this:

    double d;

    *(uint64_t*)(&d) = 4;

is hopelessly broken.  The right way that actually works, as documented in compiler manuals, is to use a union.

Also, at the very least, any compiler warnings that happen at the default warning level must be eliminated.  Not by squelching the compiler, of course, but by fixing the code.  Any virtual machine that compiles with warnings at the default level is almost certainly broken.

Now, following the rules in the POSIX world is relatively easy because the documentation uses well defined terms and such.  On Windows, however, the documentation is very often vague, ambiguous, or incomplete.  Let's just take a simple function like WriteFile().  At least, this time you know it's an actual function.  Exactly how can this function fail?  Why, even the Microsoft documentation will tell you it's not documented because there are a multitude of ways that can happen.  Compare the situation in POSIX where IO can fail for a handful of reasons, with the Windows world of system error codes that go to values like 16000 and beyond.

One such example is error code 1460, that occurs when there is a lack of system resources (whatever that means).  In practice, WriteFile() may fail with a block larger than 64 megabytes - 32 kilobytes + 16 bytes if you try to write to a shared mount.  Nice, huh?  Unfortunately this value is undocumented so you can't rely on it, and you only see the function failing if by chance you actually try such an operation.  How exactly do you write code to cope with unknown errors that happen for undocumented reasons?  So, you must keep in mind that sometimes you may not be able to follow the rules, even if you try.  This is a huge issue in the Windows world.

Here's another example from Windows.  Many APIs, such as OLE and DDE, change behavior depending on which operating system thread calls them.  So, are you sure that the virtual machine is using the same thread every time you call these functions via FFI?  How do you know this?  What if you need the virtual machine's main thread to make the calls, but Smalltalk code is running in a worker thread as it's supposed to?  Something similar happens on macOS, where the main thread receives special treatment (and must follow special rules, which means you can't run the Smalltalk system in the main thread).

Or how about calling an operating system function that fails?  In the POSIX world, the diagnostic code could be in errno, which you can't access from FFI because it could be a macro.  For this reason, some FFI mechanisms allow passing back the errno variable back to the image in a failure code temporary variable.  But errno is hardly the only convention to report errors.  In Windows, for example, you may have to call GetLastError() depending on the return value from the function you called.  The conventions are not uniform: sometimes a function fails with zero, or with -1, or with some other constant values that indicate failure (typically these values are macros, so again you can't reference those from FFI and so you're forced to hard code them, now your code is not portable, etc).  How could an FFI know whether to call GetLastError() depending on the return value of an arbitrary function? It gets even worse because sometimes there is no error reporting, and some other times the function to call may be something else, for example WSAGetLastError().  There is no way that an FFI could reasonably support all these arbitrary error reporting conventions.  But suppose that you actually called the Windows API function, and upon checking the result in the image you determined you had to call GetLastError().  Ok then, so you do that via FFI.  How do you know that between the time you called the API function, and the time you called GetLastError(), that the virtual machine or any other part of the system did not call another API function that disturbed the result from calling GetLastError()?  So even if you do this, it becomes difficult to guarantee that you will get the right result, and the consequence is random behavior.  Once more, you're pushed to write C code.

This starts having implications for the architecture of the whole system.  If you can't do a simple thing in FFI or otherwise without having to know the entire virtual machine (which tends to be a big chunk of code that is very far from trivial), or the system has many large dependencies by default, then you're effectively promoted to virtual machine engineer in one step and you have to deal with a huge amount of complexity to do anything at all.  Once more, this is why it is critical that things like virtual machines must be small, correct, follow standard operating procedures, compile with no warnings, and so on.

In turn, this also shows that the uses of FFI should be limited to the case of standalone libraries that are designed to be used as such, rather than e.g. interfacing to the operating system.  It follows that using an FFI to implement platform dependent functionality is often not the best approach.  Instead, and again in general terms, interfacing to platform specific functionality should be done at the level of the virtual machine, which is there specifically to provide platform dependent functionality and isolate the rest of the system from these dependencies.  So, for platform dependent functionality, the better default approach is to use a plugin or integrate the code (written in actual C as applicable) into the virtual machine.


## 6.  Filing bugs.

Now, it may be that from time to time you follow the rules to the letter and you still get a crash.  Perhaps you even suspect the compiler actually has a bug and such.  Then, your job is not to file a compiler bug saying that the entire Smalltalk system crashed so please fix the compiler (or the Linux kernel, or glibc, or who knows what else).  Look at all the software that runs perfectly fine, are you really sure it's not the virtual machine or some plugin code that is actually broken instead?

To prove the point, the task is to find a valid C program that has nothing except what is strictly required to reproduce the bug.  Typically these programs will be very short, and often require no header files.  If you can find such a thing, and you get a compiler or some other official platform tool to do something wrong that goes against their specification with it, then you have a good chance to file a bug that will be taken seriously.

When you file the report, it really helps to follow the proper procedure.  Such procedures will likely be documented for the program in question.  For best results, also follow the philosophy detailed here.

https://catb.org/~esr/faqs/smart-questions.html

But if it was up to you to fix a bug?  Then satisfying these conditions is imperative.

1.  You must be able to tell exactly what the bug is.  That is, it's not enough to describe the symptom ("it crashes", "the wrong color appears on the screen").  You must be able to very specifically explain what occurs, and why it is wrong.

2.  Then, you must be able to produce a set of changes to address the bug previously identified and stop it from happening.  Good thing you had a good explanation of what the bug was.

3.  Finally, this code change must be a fix.  That is, a fix is not a random change that makes the problem go away.  Instead, there has to be an explanation of why the change stops the bug from happening, i.e. you must be able to explain why the change fixes the bug in question.

If any of these are not met, then the alleged fix is no more than a change, and since the change can't be explained to work as it should then it should not be applied.

With that said, keep in mind that often the best way to fix bugs is by deleting code.

A typical complaint about FFI mechanisms is that they are slow.  Any claims of performance problems should be supported by detailed profiling information, and experience shows this is not the most frequent scenario.  Regardless, keep in mind that a proper FFI mechanism need not be slow just because it is an FFI.  For instance, nothing stops the JIT compiler from emitting code of efficiency comparable to that of code generated by a C compiler in order to call the given foreign function.


## 7.  Conclusion.

So, what have we learned here?  That the primary function of an FFI is to allow you to interface to C functionality that is given in terms of actual functions you can call, without the cost associated with using a C compiler.  One way or another, interfacing with C code means you are now a virtual machine engineer, so there are lots of things to take into consideration to satisfy the prime directive of having code that works because you can justify that it should.  This means there are a lot of things that cannot be interfaced via an FFI because of macros and other C language characteristics, or because of particular API behavior.  Of course, remember the things discussed here are just some of the factors in play.  Moreover, we also saw what happens when undefined behavior is present, and why the virtual machine should be an outstanding example of following the rules.  Finally, there is some advice for how to deal with the unexpected in terms of bug reports and how to best characterize what constitutes a fix.
