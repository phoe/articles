# Forever Stable Branch

> Stable branch, I can see you in the stable branch  
> See you again, I see you again  
> In my dreams, in my dreams, in my dreams, in my dreams  
>
> Morning light, I remember the morning li-i-i-i-ight  
> Outside my door (outside my door), I'll see you no more (see you no more)  
> In my dreams, in my dreams, in my dreams, in my dreams  
>
> [Ch.] Forever, forever I'll be, forever holding you-u-u  
> Forever, forever I'll be, forever holding you  
> Responsible, responsible, responsible, responsible, aw-aw-aw  
>
> Black and white, it's become so black and whi-i-i-i-ite  
> So insecure, you're so insecure  
> That's what you are, that's what you are  
> That's what you are, that's what you are  
>
> [Ch.]

-- [The Cranberries](https://www.youtube.com/watch?v=60DQuV6h95E)

----------------------------------

This is going to be a long post on a topic that I find exhausting to write about, and also a detour from one contracted book that I'm supposed to be writing nowadays.

In this article, I will try to do two things:

* I'll try to give an overview of how I perceive the current troublesome situation regarding ASDF and SBCL and everyone and everything else,
* I'll try to brainstorm and describe some ways forward and out of this impasse.

In this article, I won't do two things:

* I won't assume that *anything* is solved via any combination of merging one-character or one-line PRs or multiple small PRs across one repository or multiple repositories. Why? It will hopefully become obvious in the text,
* I won't assume that any kinds of personal accusations are meaningful or helpful, no matter who they are pointed at. This means in particular that:
  * If you think that pointing a finger at Stas for the whole situation is meaningful, please don't.
  * If you think that pointing a finger at Robert for the whole situation is meaningful, also please don't.
  * If you think at pointing a finger at anyone else is meaningful, ~~possibly even at me for writing walls of text about it,~~ also please don't.

To elaborate a bit more, I think it's important for me to write a few things I see about this issue [[1](http://www.winestockwebdesign.com/Essays/Lisp_Curse.html)] that has [[2](https://gitlab.common-lisp.net/asdf/asdf/-/commit/f3af2006)] been [[3](https://github.com/slime/slime/commit/2f10c1ce)] silently [[4](https://gitlab.common-lisp.net/asdf/asdf/-/commit/e3af10f0)] brewing for years in the CL world until it has started to boil, as evident in a series [[5](https://github.com/edicl/cl-ppcre/pull/30)] of related [[6](https://github.com/edicl/cl-fad/pull/24)] GitHub threads [[7](https://github.com/edicl/flexi-streams/issues/25)], a mail [[8](https://mailman.common-lisp.net/pipermail/asdf-devel/2022-January/006680.html)] from the current maintainer of ASDF, a Reddit thread [[9](https://www.reddit.com/r/Common_Lisp/comments/s02yhz/stas_has_alienated_longtime_asdf_maintainer/)], a blog post [[10](https://www.timmons.dev/posts/cl-community-norms.html)], another Reddit thread [[11](https://www.reddit.com/r/Common_Lisp/comments/s6eajb/cl_community_norms/)]... and, chronologically, it seems that this article as well now that it's public.

Oh well.

Buckle up, let's get going. There's a long path to walk, and there's plenty of detours to take to convey all the information I want to say.

-------------------------------------------

The current CL ecosystem is a pretty interesting place. If you're a Lisper, you might know what I mean; if you're not, then I'll try to involve a little bit of context along the way as I write.

But, let's start with a hot take instead. 

Hot take: CL is actually a single-implementation ASDF-centric programming language. All that sometimes differs is user code and ~~Lisp implementations~~ standard libraries and compiler plugins.

C'mon, prove me wrong.

But wait, there's more! The "single implementation" in question is, in fact, not a single implementation, but a [mud-ball](https://en.wikipedia.org/wiki/Big_ball_of_mud#In_relation_to_Lisp) of de-facto standards which have organically emerged since ANSI CL happened 28 years ago.

This mud-ball includes software like [ASDF and UIOP](https://gitlab.common-lisp.net/asdf/asdf), [Quicklisp](https://www.quicklisp.org/beta/), [CFFI](https://github.com/cffi/cffi/), [Bordeaux Threads](https://sionescu.github.io/bordeaux-threads/), [Closer to MOP](https://github.com/pcostanza/closer-mop), [USocket](https://github.com/usocket/usocket), [Swank](https://github.com/slime/slime/tree/master/swank), [Static Vectors](https://github.com/sionescu/static-vectors/), [Trivial Garbage](https://common-lisp.net/project/trivial-garbage/), [Trivial Gray Streams](https://github.com/trivial-gray-streams/trivial-gray-streams), [Alexandria](https://gitlab.common-lisp.net/alexandria/alexandria/), [Ironclad](https://github.com/sharplispers/ironclad/), [Babel](https://github.com/cl-babel/babel/), [CL-Unicode](https://github.com/edicl/cl-unicode/), [named-readtables](http://melisgl.github.io/named-readtables/), [split-sequence](https://github.com/sharplispers/split-sequence/), [Nibbles](https://github.com/sharplispers/nibbles/), [Flexi Streams](https://github.com/edicl/flexi-streams/), the list goes on and on and on. 

* *Exercise: Do `(ql:quickload :quicklisp-stats)` and `(subseq (quicklisp-stats:month 2021 12) 0 n)` for some value of `n` and get your own data on that one.*

Also, before you ask: yes, I've invented that take myself, so all blame goes straight onto me.

But still. Come on. Prove me wrong.

---------------------

After cooling down a bit, it's easy to see at least two reasons for which this take is unfair.

* One - there exist CL-based software systems which do not use a majority, or even *any*, of the aforementioned libraries, thoroughly debunking that take in an instant;
* Two - it is unfair to assume that every piece of software mentioned by name is equivalent when it comes to different *versions* of that software. This includes ASDF as well as other software that depends on it for loading.
  * *Thankfully, since the prevailing attitude with regards to maintaining CL code is to maintain backwards compatibility, this second issue is mostly limited to forwards compatibility in practice.*

Still, there's at least a grain of truth in that hot take, and I consider that grain to be relevant to the part of the Common Lisp world that I am the closest to - which is the mostly-FOSS-centric group of Lisp hackers currently gathered around `#commonlisp` on the Libera Chat IRC network, and [/r/Common_Lisp](https://reddit.com/r/common_lisp), and [the Common Lisp Foundation](https://common-lisp.net).

I'll refer to that part of the ecosystem as my "CL world", and I'll also use ASDF as an example library for the rest of the article and skip the other systems. (Trust me, there's still be a lot left to write about after limiting my blogpost to these.)

---------

So! In order to rectify the second issue with my original hot take, it would be good to clarify what *exactly* is meant by the term "ASDF". What concrete artifact of the CL world is being referred to when I, or Lispers in general, say "ASDF"?

To answer this question, I'll use two terms across this blogpost - "ASDF-stable" and "ASDF-devel". Wherever I use the term "ASDF", I refer to both of them; otherwise, I explicitly mean one or the other

Regarding **ASDF-stable**, on one hand, it's possible to state that this term refers to version 3.3.1, which is the last version shipped by default with SBCL; on another, it's possible to state that it's 3.3.3, the last version shipped by default with CCL; on yet another, it's 3.1.1.8, the last version shipped by default with ECL, but on yet yet another, it's 3.2.1, because Quicklisp ships that!...

The concrete version that we settle on thankfully doesn't matter *too* much for the conceptual exercise that I want to do in this post. So, let's "simply" say that it means "the old ASDF" without specifying its concrete version.

**ASDF-devel**, on the other hand, refers to whatever version is relatively *newer* or even *newest*; at the moment, the newest is 3.3.5.7, but that's going to change.

----------------

Thankfully, ~~or not,~~ we don't have much of that problem with versioning Common Lisp. CL, as a standard, is frozen in time ever since 1994, and the only things that update its versions are its various implementations. And all versions of the various implementations are meant to pass the [ANSI test suite](https://gitlab.common-lisp.net/ansi-test/ansi-test/) (and optionally skip the tests for the debatable parts of the standard) if they purport to conform to ANSI CL.

D'oh, sounds too simple to be true, doesn't it?

As mentioned earlier, there are many functionalities which are helpful for writing programs that are *actually useful* - and yet the ANSI CL standard does not specify them. Many such extensions are not standardized in *any* way. They are instead meant to be provided by implementations themselves, and implementations which claim to be actually useful also provide these extensions in various shapes and sizes. 

You know, trivialities like calling into and from foreign code, networking, multiprocessing, interfacing with the garbage collector, user-programmable Gray streams, the Metaobject Protocol, character encoding and Unicode, environment variables, image dumping, filesystem access, pathname utilities, et al.

In order to make something that's at least *resembling* portability and standardization, some Lisp hackers have begun working on uniform interfaces that abstract over the implementation differences - and we have the wonderful world of ~~portability~~ compatibility libraries which form the de-facto standard of the contemporary CL ecosystem.

There's just one notable exception. There's no compatibility library for loading Lisp software - and that's because there is only one widespread modern Lisp system loader, with only one implementation which also specifies its API and behavior, and - like almost everything else in the Lisp world - cannot have different versions of it loaded into the same image (since [SICL first-class global environments](http://metamodular.com/SICL/environments.pdf) are not yet practically usable).

That's ASDF.

And that's specifically why I chose it as an example for this article. And that's also the grain of truth from my original hot take.

----------------------

Since there are no stupid questions - why is backwards compatibility even important? I will not answer this directly and instead point to some examples existing in the CL ecosystem. The ANSI CL specification is the obvious one, ensuring that portable code from 1994 runs without modifications on ANSI CL compilers of 2022, even if some stylistic choices from that time are dubious. Some Lisp code from *before* ANSI CL can also be easily ported to ANSI CL that way, since ANSI CL is, itself, *mostly* backwards compatible with other Lisp dialects that preceded it.

But let's try to find an analogy in some of the existing projects. One example that I thought of is [CL21](https://github.com/cl21/cl21) - a project that rewrites all of the Common Lisp standard library and includes many of the de-facto standards, currently implemented by separate libraries, into the new standard library.

It was quite loud back in the day, gaining some strong proponents and some strong opponents alike, and which still lives on [GitHub](https://github.com/djr7C4/cl21) and runs on modern CL implementations.

One aspect of CL21 that is important in this context is: when attempting to load and use code written in CL21, users of standard CL don't need to specially care about it. To them, CL21 can be - and often is - just another CL dependency that will be compiled and loaded as a part of loading the system that depends on it and that CL code can normally interface with.

This similarity does not hold once we substitute ASDF-stable for CL and ASDF-devel for CL21. In particular, ASDF-devel is *not* backwards-compatible with ASDF-stable, and making the jump requires user intervention to resolve incompatibilities - but, more about that later.

----------------

Another good example of maintaining backwards compatibility is [Alexandria](https://gitlab.common-lisp.net/alexandria/alexandria/) and its inability to extend the exports of its main package in any meaningful way.

Why, one would ask? We have package-local nicknames nowadays. It's possible to avoid any `:USE` and simply `(:LOCAL-NICKNAMES (#:A #:ALEXANDRIA))`. What would really happen if Alexandria attempted to add more exports?

Well, you know. [This](https://github.com/search?l=Common+Lisp&q=%22%3Ause+%3Aalexandria%22&type=Code) would happen.

And that's just the visible part that is available on GitHub and not on any GitLab or Gitea instances or on any private Git repositories.

In the best case, if another package is also used alongside Alexandria and if it exports symbols that are named the same, there would be naming conflicts and the projects that `:USE` Alexandria in such a way would no longer build.

In the worse case, there would be no such project, but the project in question defines an operator with the same name itself. If Alexandria added a new export named e.g. `FOO` and our project both `:USE`d Alexandria and has a `(DEFUN FOO ...)` somewhere in its code, we are not only overwriting the function for ourselves, we are also destroying the previous function binding of `ALEXANDRIA:FOO` for all projects that decide to make actually meaningful use of this new export.

If Alexandria decided to change its exports ~~in an incompatible manner~~*at all*, then it would be required to review and fix **all** of the code that makes `:USE` of it right now. That's a textbook example of breaking backwards compatibility, and the most important liability of Alexandria maintainers right now.

The "come on, it's just one symbol" approach is something that will neither reliably work nor reliably scale: even if adding just one symbol miraculously causes no side effects in the CL world, there is no way to prove that adding another symbol will work the same way. Oh, and also, since Alexandria is the most popular library on Quicklisp, that previous sentence should say, "causes no side effects in the CL world *that Alexandria maintainers will ever become aware of*".

And, to this problem, there are no "true" solutions, only backwards-compatible compromises and workarounds - such as the `ALEXANDRIA-2` package currently defined by Alexandria that exports all symbols from`ALEXANDRIA` plus extras that are not definable there. Bordeaux Threads [is going the same way](https://github.com/sionescu/bordeaux-threads/blob/master/apiv2/pkgdcl.lisp), and one of the next releases is going to introduce `BORDEAUX-THREADS-2`. I'll likely see other Lisp systems following suit in the future.

-----------------

Let's take a step away from the tools and backwards compatibility and talk about the people.

Or, should we, really?

That's an important question to answer in context of what, at least in my opinion, sets the CL world from some of the worlds of other programming languages. In these worlds, there are plenty of people, more or even *much much more* than the headcount of people doing Common Lisp.

Along with so many people, and therefore many projects, there is some emergent or explicit hierarchy with decision chains, as well as lengthy discussions about directions, practices, architecture design, versioning, and so on, often made by some sorts of working groups, standards committees, or benevolent dictators for life. Java, C++, Python, these are the ones that first come to mind.

I'll call these worlds *people-oriented* - mostly to set the CL world apart, because it is very much *not* one of them. I'll call the CL world *tool-oriented* - and that is, to summarize, because CL, when used by a programmer as a *tool*, makes it possible to be pretty independent from *people* in general.

This fact comes with both (obvious) benefits and (less obvious) costs - and more about these in a moment, after we make a tiny detour.

-----------------------------

If you had the misfortune of digging into the threads linked in the beginning of this article, or if you had the even greater misfortune of seeing some of the other posts and threads and articles made during the drama winter of 2020/2021 when one peculiar self-proclaimed messiah had to be shown the door (if you care about gossip: no, he still doesn't seem to have changed any of his old ways), then you will surely recall some comments that show the sentiment of "there is no such thing as a Common Lisp community".

I'd like to elaborate on that sentiment a little bit more, since it's a thing that has been repeated for decades in multiple different ways. I think it directly refers to the fact that, in the CL world, there is no direct analogue of the *people-oriented* worlds known from other programming languages.

Instead, what we have is a *tool-oriented* world that is centered on *artifacts* rather than *people*, on the "what" rather than the "who" - and that includes the CL and MOP specifications, Lisp implementations, compatibility libraries and popular pieces of software. People come in, pick up the tools, do their work, and either leave after their work is done or stay around and listen, chat, talk, ask questions, which is superficially similar to what happens in these people-oriented worlds of other language.

I'll risk being called a borderline circle-jerker here, but CL is one of the few languages where long-term stability is possible, feasible, *and* actually exercised in practice *because* it is possible to use it without direct dependence on other people. CL, as a tool, both has most of its specification frozen in time since 1994 *and* is still useful in modern times even after almost thirty years *and* can be successfully adapted and reinvented over [[1](https://github.com/clasp-developers/clasp/)] and over [[2](https://github.com/robert-strandh/SICL/)] and over [[3](https://github.com/froggey/Mezzano/)] again in modern times. That, alone, is pretty damn magical in itself, and a proof that X3J13 has done a marvelous work specifying CL.

Where other languages depend on personal sources of authority like BDFLs or design committees, the Common Lisp standard and ecosystem has proven itself *both* timeless and useful when it came to software stability. Where other languages die because the *people* who make up their worlds move on to greener pastures or die out, Common Lisp, as a *tool*, instead becomes "dormant" - and that term is also [somewhat widely recognized](https://twitter.com/nihirash/status/880829816072802304) in the programming world.

"It's undead", "it's timeless", "it's dead, Jim", "it isn't dead, it just smells funny", "just add water and stir", "Ruby is an acceptable Lisp too", and so on - all of these jabs and takes share the same sentiment, regardless of whether it is to poke fun at CL or to try to express this particular quality of the language and its ecosystem that causes it to hang in that everlasting gravitational trap between condemnation and immortality.

All of the above also causes a little bit of survivorship bias in both types of programming worlds: people who *need* larger gatherings of people for some reason tend to flock elsewhere, whereas the CL world is capable of attracting and keeping people who want or need to program in tiny groups or even complete solitude.

---------------

The above trait is a major contributor to the way in which one's mind can have a visceral relationship with their Lisp image. In the CL ecosystem, it's possible and feasible for single users to make major design decisions regarding the software they write that would take multiple people to even execute elsewhere. Whereas in some languages the practical choice is "which framework do I use", a CL programmer can *also* make a choice of "which framework do I modify" or even "which framework do I *make*" - simply because the language permits them to easily do so.

* *This approach bears its fruit in many areas, including those in which Common Lisp has a relative abundance of libraries, such as testing frameworks and JSON libraries - but that's a topic for a whole separate article.*

A longish intro, eh? All of this has already been said elsewhere multiple times. Let's see how ASDF ties into all this. In particular, let's begin with ASDF-stable.

As much as people have complained about ASDF-stable in the past, it turns out that it mostly stood the test of time. All of the Quicklisp ecosystem is based on ASDF-stable. An unknown number of closed-source systems and private code are based on ASDF-stable, even more so if they use any Quicklisp-provided free software.

We've mentioned earlier that CL as a tool is timeless, in a way; well, ASDF-stable has certainly joined its neighborhood. A combination of both of them is *good enough* for contemporary use on one's own, and that's one of the major factors that decides on a tool's success.

How can such success be measured? Well, one approach would be to measure how widespread a tool is, or, in other words, how many users it has. In case of CL, doing this is already pretty hard: there are no official metrics, there is no user tracking (and thank goodness for that), there are no public download statistics that are comprehensive to make any metrics off.

GitHub can give us a little bit of metrics via its code search feature, but we've already realized it isn't comprehensive enough to give us full information when we searched for projects that `:USE` Alexandria. Free Lisp implementations are widespread as all free software is, and the commercial Lisp vendors may not be very eager to share their trade secrets with the broader public. Same stuff with individuals and companies who merely use Lisp - they seem inactive when it comes to ASDF development, or in the free software Lisp world, or we aren't even aware that they use Lisp at all.

ASDF-stable, as delivered with most free Lisp implementations and Quicklisp, follows the same rule. It's damn hard to measure the users of it. And, because of that, it's also damn hard to measure what kind of impact a backwards-incompatible change is going to have *if* that change ends up attempting to trickle downstream.

---------------

The harsh reality is that the developers of ASDF-devel have made a change that was at the same time proclaimed to be backwards-compatible by developers of ASDF-devel *and* non-backwards-compatible by the factual users of ASDF-stable, and no amount of minor versioning on ASDF-devel's side is going to change the position of users of ASDF-stable, since the users are, by definition, [always right](https://xkcd.com/1172/).

The behavior in question is possible to turn off, but it is the defaults that matter, because in already existing projects it is the defaults that are used and the defaults that will break builds - and it's possible that even a single warning coming from ASDF-stable's `load-asd` is enough to break a build, as according to [Hyrum's law](https://www.hyrumslaw.com/).

The defaults matter because it takes a much greater amount of energy to go from "we need to change nothing to make this build again" to "we need to change one thing to make this build again" than it takes to go from "one thing" to "two things" or even "five things".

The reasoning used by ASDF-devel maintainers and the replies used by people who want backwards compatibility could be summarized like this:

* **Dev:** We now fully support defining multiple systems in a single .asd file! To make use of it, you need to use a particular delimiter in your system names.
* **User:** Our code now signals a warning when we `load-system`.
* **Dev:** Oh, you must have been using undocumented behavior of ASDF. Your code will continue to work as before, but if you change this `#\-` to a `#\/` it'll stop complaining.
* **User:** Our code used to work in ASDF-stable. Any warning at all breaks our build, so it's broken in ASDF-devel. Just make it work like it did in ASDF-stable again.
* **Dev:** The only way we know to support the improvements we want and make `(load-system "foo-test")` work without `(load-asd "foo.asd")` first would be to read all .asd files. It'd be a major performance regression.
* **User:** That's normal behavior for us, we are used to running `(load-asd "foo.asd")` first. And we shouldn't need to care about the filesystem-walking algorithms used by ASDF.
* **Dev:** Okay then, please make your suggestions on the ASDF-devel mailing list.
* **User:** We have no time/will/money/X to do that, sorry. We'll just stick with ASDF-stable.
* **Dev:** If we just get rid of the warning, it's likely that people will keep writing new code that depends on undocumented behavior.
* **User:** That undocumented behavior works well for us, though.
* **Dev:** That behavior that is a burden for us as ASDF maintainers, and we want to clean up our code and stop supporting it altogether.
* **User:** Does breaking backwards compatibility with our pre-existing projects *justify* the improvements you gain from cleaning up your code in the first place, then?

----------------------

One of the long-lasting conflict zones is about the default version of ASDF bundled with implementations, especially SBCL. I can perfectly understand SBCL's stance here.

If SBCL, by default, upgrades ASDF-stable to ASDF-devel, the issue is pushed one step closer to the end users of CL software systems. I'll repeat myself from one of the previous posts - it's understandable that SBCL wants to avoid getting the fallout for shipping an incompatible change in a contributed module and therefore breaking builds of people who "just" upgraded SBCL to a newer version. This has happened in the past, and it is already known that this will happen in the future.

This already touches an important distinction that we're yet to make - namely, we've repeatedly used the term "users of ASDF-stable". Who is that exactly? It's hard to know for sure. On one hand, all current users of ASDF-devel (who exactly is that - we'll describe in the next paragraph) are also, for the time being, users of ASDF-stable, since the incompatibilities between the two are not all that large. On the other, since it is impossible to tell how many users of CL there are, it is similarly impossible how many of these users were shipped a copy of ASDF-stable.

So, if we follow up with the who-uses-it question from the paragraphs above, we can extend it further and ask, "who are the users of ASDF-devel?" and "who are the *concrete* people that the ASDF-devel team develops for?" It's still possible to answer these questions with relative ease, but that is *only* because ASDF-devel is not yet widely disseminated and it has spread mostly in circles that are reachable from the position of an ASDF maintainer or a CL hacker working mostly in the FOSS circles.

The harder issue is that, outside of the Quicklisp world, we have no idea which of the current ASDF-stable users can *smoothly* become users of ASDF-devel, with "smoothly" as in a "smooth upgrade". Without that knowledge, it's impossible to prepare any kind of upgrade paths for them, which can include any custom ASDF settings that can e.g. turn off some warnings just for them and not for everyone else. Because of that lack of knowledge, the only way that can make upgrades smooth for *sure* is a backwards-compatible default setting.

---------

Yet another take is, we're in a world of free software. We're exchanging gifts with one another. They are fully no-strings-attached when we write code that's in the public domain, we promise to tell everyone who we got the gifts from when we do MIT licensing, we promise to share our contributions back with other people in various configurations when we do LLGPL, GPL, AGPL and so on.

The person who accepts the gift doesn't need to do anything more. The person who accepts the gift isn't entitled to anything else than the gift itself, either. 

This means that the authors and developers and maintainers of ASDF aren't in any way required to keep ASDF-devel fully backwards-compatible with ASDF-stable - the gift from Team ASDF is all the various versions of ASDF that have ever been produced and given away for free with no strings attached other than attribution. No one can prevent them from having their way, and that's a fact.

There is the other side of the coin, too, and for some reason it is much harder to deal with than the first one. It's that users of ASDF-stable aren't in any way required to *ever* upgrade to ASDF-devel. ASDF-stable was a gift, and they're free to use it for whatever goals and for however long they'd like to. No one can prevent them from having their way either, and that's a fact.

-------

I mentioned that I had a longish intro before. Well, sure I wasn't lying; lots of context that I consider necessary to be said before proceeding.

You see, the *real* issue that I see is that people behind ASDF-devel are not currently fighting with SBCL maintainers, it isn't fighting with old system definitions, it isn't even fighting with users of ASDF-stable.

It's fighting an uphill battle with ASDF-stable itself.

ASDF-stable, as a piece of software, has proven itself to be good enough for its users, and ASDF-devel is suffering from the success of its own earlier version. Because of a relatively small change that nonetheless breaks backwards compatibility, ASDF-devel can be treated as, in a way, a new, separate, and *mostly compatible* system definition facility, attempting to usurp the current de-facto standard - ASDF-stable.

And that's some big shoes to fill.

This "can be treated as" from the above paragraph is the main reason why I've separated ASDF into ASDF-stable and ASDF-devel at the beginning of this article. It's not unlike the situation that Microsoft had with Windows XP when Vista was first introduced: the old version of Windows was so useful and *venerated* that it was hard for Vista to gain any traction, and traces of this trend remain even *decades* after Microsoft has pulled the plug not just on XP, but on Vista, 7, and 8 alike.

Also not unlike Python 2 being unable to die for years and still unable to fully die nowadays despite being end-of-life for a considerable while now.

Also not unlike Linux kernels 2.6 and 3.10 which not only refuse to die nowadays but also have whole corporate teams dedicated still to supporting them and creating and applying patches wherever necessary.

In a way, Windows XP and Python 2 and Linux 2 and Linux 3 eat and/or ate their children - and so did, and do, both [Smalltalk and Lisp](http://worrydream.com/EarlyHistoryOfSmalltalk/). This includes Common Lisp in particular - see Parenscript, CL21, Coalton, etc..

The people behind ASDF-devel aren't Microsoft, though. There is no real way to pull the plug on ASDF-stable and no real *need* to do this either - at least from the users' point of view.

In a way, this issue looks like ASDF-devel versus Hyrum's law, or a practical workshop in software ossification. If it was merely *possible* to define test systems with names like `foo-test`, then it became a part of the public API of ASDF-stable, and ASDF-devel can be either compatible with this behavior or not.

The only known sure-fire solutions to Hyrum's law are to make a backwards-incompatible fork that is capable of removing the cruft, or to be fully backwards-compatible. There's simply no other way to be 100% certain.

------------

So, here we are. The combination of ASDF-stable and SBCL, the most prominent combination of CL implementation and system loader, has gained widespread traction and use *and* they are currently good and stable enough to remain in use.

What is the current situation with ASDF-devel though?

At the time of writing this article, ASDF-stable was released 4 years and 2 months ago. What has changed since then, feature- and bugfix-wise?

Let me try to make a short list.

* Support for package-local nicknames in package-inferred systems and `UIOP:DEFINE-PACKAGE` was added - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/cffbda7a), [2](https://gitlab.common-lisp.net/asdf/asdf/-/commit/85dedfeb), [3](https://gitlab.common-lisp.net/asdf/asdf/-/commit/61e3e45e), [4](https://gitlab.common-lisp.net/asdf/asdf/-/commit/caf89a8a), [5](https://gitlab.common-lisp.net/asdf/asdf/-/commit/353c4cf9), [6](https://gitlab.common-lisp.net/asdf/asdf/-/commit/823c389f), [7](https://gitlab.common-lisp.net/asdf/asdf/-/commit/d9ce5417), [8](https://gitlab.common-lisp.net/asdf/asdf/-/commit/1a453bac);
* Support for package locations on SBCL for packages defined by `UIOP:DEFINE-PACKAGE` - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/8281e01), [2](https://gitlab.common-lisp.net/asdf/asdf/-/commit/ed0f4d5e);
* `UIOP:DEFINE-PACKAGE` options `:USE-REEXPORT` and `:MIX-REEXPORT` were documented - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/bf589e14);
* Extension points for component parsing were added - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/aea4917b);
* Warnings or errors are signaled for `:IMPORT-FROM` when non-symbol designators - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/4bb3b11d);
* Support for Mezzano was added - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/3a4e2407);
* Support for Genera was added - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/0046d995), [2](https://gitlab.common-lisp.net/asdf/asdf/-/commit/6cc44d02), [3](https://gitlab.common-lisp.net/asdf/asdf/-/commit/a92cd99d), [4](https://gitlab.common-lisp.net/asdf/asdf/-/commit/1173263c), [5](https://gitlab.common-lisp.net/asdf/asdf/-/commit/d6d7d423);
* Support for ECL was improved - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/a6472e1e), [2](https://gitlab.common-lisp.net/asdf/asdf/-/commit/0e0b1f88);
* Support for Clasp was improved - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/30a4cd6c), ..., [14](https://gitlab.common-lisp.net/asdf/asdf/-/commit/154ff568);
* Support for LispWorks was improved - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/34308928), [2](https://gitlab.common-lisp.net/asdf/asdf/-/commit/c53cca18);
* Support for ABCL was improved - [1](https://gitlab.common-lisp.net/asdf/asdf/-/commit/445eba4e);
* ...

On one hand, that's not bad; I skipped tens of bugfix commits that I was unable to reasonably summarize here or that I haven't been able to pull out of the ASDF changelog during a one-hour-long search. I also skipped many bugfixes related to keeping ASDF and UIOP transparently upgradeable from older versions to newer, which is an ASDF priority.

I also skipped as well as multiple commits related to keeping ASDF tested on multiple implementations and configrations in CI loops; the list should be at least three times as long as it is right now to account for all that. I don't think it's hard to notice the work that the people maintaining and extending ASDF keep on working to keep the build system working and flexible for everyone's use.

On the other hand? The factual users of ASDF-stable *don't need to, and likely won't, care about any of these things*. No support for package-local nicknames in `UIOP:DEFINE-PACKAGE`? It's *easy* to do one's homegrown `DEFMACRO DEFINE-PACKAGE` that will expand any `:LOCAL-NICKNAMES` option into an `EVAL-ALWAYS` over `REMOVE-LOCAL-NICKNAME` and `ADD-LOCAL-NICKNAME`. No support for Clasp or LispWorks or Genera? We use SBCL or CCL, we don't need them. No support for jump-to-package-location? Who even needs that, we have `grep`. No bugfixes for rare and/or edge cases? Restructure one's code to avoid hitting them.

The users of ASDF-stable do not even need any maintainers *exactly* because of *how good ASDF-stable is* for their needs and because, if one absolutely needs to delve into one's own ASDF branch, it's possible to either fix an issue there or cherry-pick single commits from the main tree. (Remember the "fan-made security patches" and "unofficial service packs" for Windows XP? In case of ASDF-stable, you don't even need to reverse-engineer the binaries.)

It's noteworthy that peculiarity has emerged due to no real fault on the side of the ASDF team or SBCL team. At least to me, it seems that ASDF-stable has reached a certain evolutionary optimum in which the costs of upgrading to ASDF-devel *are*, in a non-trivial chunk of real-world situations, unfounded. In some situations that is because ASDF-stable is good enough for whatever it does right now, but in some, because upgrading to ASDF-devel would require reconfiguring it to be backwards-compatible by default.

-------------

It's also perhaps easy to conflate ASDF-stable and ASDF-devel into one project (everyone keeps on doing it, after all!) and therefore fall into the trap of thinking that they are also the same *product*.

Nope. They are obviously not.

One of them is used as something that maximizes stability and is meant to work as it does *without* introducing any breaking changes in order to maximize confidence in the developer environment; the other is meant to be a vehicle for evolving Lisp software beyond what it currently is and changing its structure in ways that are incompatible with the past and the current but are meant to bring long-term benefits in the future.

They are not the same.

-------------------

The existence and veneration of ASDF-stable is the thing that, as I imagine, is the hardest for the current team of ASDF-devel to handle, and the core of the current issue. ASDF-stable turned out to be so good that it has joined Common Lisp in its *timelessness* - and it means that it's there to stay more or less forever.

This is because of the seeming nature of the contemporary Common Lisp world which, effectively, is, at its core, an append-only environment. It's a place where deviations from the rule of "don't break existing code" are few and far in between, and this approach continues to bring situations where versioning Lisp systems is generally *not* necessary because of the heavy weight that its users place on backwards compatibility.

Yet another hard issue, stemming from the above, is that ASDF no longer refers to a single piece of software. It has already been forked into ASDF-devel and ASDF-stable - not by its developers, but by its users, and that split is something that, IMO, shouldn't be ignored.

One way forward that I see is to *acknowledge* that fork and actually give it a name:
* either via software versions, which would imply that the first factual versions of a backwards-incompatible ASDF 4 are already present and used in the wild
* or across a whole project, which has a much larger organizational overhead.

In the worst case, if that approach is *not* taken, I expect that there will be either forks of software systems for satisfying two distinct and mutually incompatible version ASDFs, or single software systems with twin system definitions in the tougher case of a full project fork. (We've already been through that in the times of early ASDF, where `MK:DEFSYSTEM` and other loaders reigned.)

Regardless of which approach is taken, ASDF-stable is with us to stay, simply because there is a nontrivial number of CL users who value backwards compatibility *over* mainstream updates or upgrades, and who feel more confident to roll their own if they ever need to do that. And that won't change.

--------------

Epilogue time, or something?... I've already written, like, a ton of text.

In a way, I see the contemporary CL world as a practical example of a postapocalyptic tribal reality.

Postapocalyptic, because the current CL ecosystem does stand on the shoulders of giants as much as it stands on their bones, as evident in the Lisp AI Winter during which many Lisp programmers turned to do something else and much of Lisp progress and knowledge was either absorbed into other programming languages or became lost and left to be (re)discovered. The consequences of that Winter are still something that echoes around the CL world in early 2022 and something that won't calm down easily.

Tribal, because it is both *possible* to create small and self-sufficient tribes and *feasible* to stay in them because of the low population. Possible, because Common Lisp is a true *artifact of the old times* - a tool that is both useful for small groups of programmers, as small as one, and good and future-proof *enough* to build software in; feasible, since other languages have absorbed a lot of CL functionalities and because of the global Lisp cooling that happened after the AI Winter.

Reality, because it's nothing that is pulled out of a book of some sort. It's early 2022 and I get to experience it daily as one of the living parts of the CL ecosystem. And all I can do is document it and do a little bit of people work to try and figure out a way forward to avoid yet another tribal war which is brewing in the distance. Because, as in all tribal worlds, tribes sometimes go to war with each other, and all you can do as one of the tribesmen is to try and convince them not to.

And then you don't fully control the outcome. In a tribal world, it is clear that no one does - whereas in a hierarchical people-world it's possible to at least pretend that they do - until the people-oriented worlds die out and/or the tool-oriented worlds go into another hibernation.

But, yeah. Until that happens, we're here. And there will always be to sing the chorus:

> [Ch.] Forever, forever I'll be, forever holding you-u-u  
> Forever, forever I'll be, forever holding you  
> Responsible, responsible, responsible, responsible, aw-aw-aw

-- [The Cranberries](https://www.youtube.com/watch?v=60DQuV6h95E)

--------------------------

OK. That's all for the first part that I wanted to write about.

D'oh. Yes, obviously there's a second part. Didn't I mention it's a long ride?

# Forever Stable Branch 2: Electric Boogaloo

Part 1 describes the current reality, harsh and tough sometimes, the way my logic sees it. A big and incomplete personal summary of it could be: as much as it personally saddens and frustrates me, backwards-compatible defaults **should** stay until ASDF evolves to a new major version, in order to preserve backwards compatibility mandated by semantic versioning. And by compatibility, I mean the factual one defined by Hyrum's law, the behavior that known and discoverable real-world users depend on; ASDF-devel purports to be backwards-compatible with ASDF-stable, so consciously doing things that break such compatibility in practice simply makes no sense.

Now that it's out of my system, though, it's time to get a little bit more personal, and to let my heart have a voice as well.

I'm actually rooting for Team ASDF. I hope that they get to bring some more structure to test system definitions and that they'll get to polish some of the rough edges that CL system definition currently has.

Yes, that's in spite of all of the above things that I've written, even if it may (and I expect it will) quirk a brow or two. Actually, I wrote it both for two reasons:
* to give an overview of the current situation as I perceive it,
* because I think that a commented collection of all already known obstacles and blockages that are ahead of executing ASDF's long-term plan may become useful at some point.

If you don't consider me bonkers yet, read on. If you do, then you're free to do so anyway.

---------------

Story time. (I apologize, I tend to write volumes. Especially when I'm supposed to be working on books.)

You're very likely acquainted with a piece of software called [Slime](https://github.com/slime/slime/), which is a piece of Emacs code meant to facilitate interactive development in CL. It comes with a Common Lisp backend called [Swank](https://github.com/slime/slime/tree/master/swank/).

A peculiar thing is, if you try to load an older version of Swank ([2.19](https://github.com/slime/slime/tree/slime-2.19/) is old enough) on a modern SBCL, you'll end up in the debugger with an error reporting as `Symbol "%SIMPLE-FUN-NEXT" not found in the SB-KERNEL package.` The only solution is to upgrade Swank to a newer version.

The thing is that Swank used an [internal](https://github.com/slime/slime/blob/v2.19/swank/sbcl.lisp#L1591) symbol despite the warnings associated with using a symbol via the ~~in~~famous `foo:%bar` syntax *and* despite accessing a symbol from an internal package like `SB-KERNEL`. Since the symbol was clearly internal, SBCL was right to be able to remove it whenever suitable, which finally [happened](https://github.com/sbcl/sbcl/commit/e103be07) - and Swank had to be [updated](https://github.com/slime/slime/commit/840e013d) to make it happen.

Still, *not* using that internal symbol, or many other internal symbols (the aforelinked file lists 27 hits when searching for the string `SB-KERNEL`) would mean that Slime would miss a lot of SBCL-related functionality. It was a choice of either hacking something that is useful and therefore produces value for many SBCL hackers, or not doing that and waiting for SBCL to release some official interfaces to its guts.

So, in other words, a choice between a dumbened-down Slime, and exercising Hyrum's law in practice. Because the latter was chosen, Slime had working and useful compiler conditions, descriptions, and inspector that served many Lisp hackers over the years.

And still, people keep on hitting that issue nowadays. I got to help one such person not too long ago, even; told them to update Slime, bam, everything works. Still, somebody had to recognize the error and be there to take proper steps. That's an example of practical cost of breaking backwards compatibility.

Lesson learned? If *even* SBCL is capable of breaking backwards compatibility with *factual* real-world users of its software (and it isn't the only time it happened - count all the occurrences of `{minor ,}incompatible change` in the SBCL changelog), then it means something.

-----------------

Okay. Story time, take two.

See this code snippet?

```lisp
(defstruct foo (x 0 :type string))
```

Seems like a normal bit of Lisp, huh? Maybe a little bit in the undefined behavior zone if `(make-foo)` is ever called instead of `(make-foo :x ...)`. Oh, but there's no complains from SBCL when we evaluate that though. Fine, good enough.

Well then, compare that to this:

```lisp
(locally (declare (optimize (safety 0)))
  (defstruct foo (x 0 :type string)))
```

The only difference is that we've decided to live on the edge and disable runtime safety checks.

What do we get?

```
; in: LOCALLY (DECLARE (OPTIMIZE (SAFETY 0)))
;     (X 0 :TYPE STRING)
; 
; caught STYLE-WARNING:
;   Derived type of (X 0 :TYPE STRING) is
;     (INTEGER 0 0),
;   conflicting with its asserted type
;     STRING.
;   See also:
;     The SBCL Manual, Node "Handling of Types"
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
```

Huh. Interesting! There seems to be some sort of compile-time type-checking that is happening on zero safety, but - perhaps counterintuitively - *not* on higher safety settings.

What's the story here?

The check was introduced in [SBCL 1.5.9](https://github.com/sbcl/sbcl/commit/bac9b5e7a) and originally was meant to work on all safety levels.

And then, you know. All hell broke loose in the Quicklisp world that tried to build the world with that check compiled in SBCL.

The [IRC log of `#sbcl`](https://irclog.tymoon.eu/freenode/%23sbcl?around=1574802609#1574802609) from that time period describes some of the fallout. Turns out that **lots** of projects had `DEFSTRUCT` slot forms similar to `(stream *standard-input* :type keyword)` (remember the last time `*STANDARD-INPUT*` was a keyword?) or `DEFCLASS` slot forms like `(div :type (integer 2) :initform 1)` (remember the last time 1 was at least 2?).

This situation was made possible because the initforms did not have their types checked against the declared slot types before. These bugs (because these are all type specification errors!) were, in most cases, dormant and uncaught before - until SBCL, in its initial eagerness, highlighted them *en masse*, and effectively broke a lot of Quicklisp world of that time which *depended* on these types to *not* be checked in doing so. Hyrum's law in practice, again!

The immediate reaction was that SBCL was *forced* to "revert" this warning [also in 1.5.9](https://github.com/sbcl/sbcl/commit/1513ac241), right before release. The "revert" in question works by only exhibiting the warning in code compiled with zero safety. This way, zero-safety code that has no run-time type checks (and thank goodness, there isn't a lot of that on Quicklisp) can benefit from these compilation-time warnings and code that has run-time checks can instead build in peace.

* *As an ~~un~~intended side effect, compiling your code with zero safety can give you more compiler output than compiling it with high safety. Not obvious in the slightest, but that's how it is.*

The commit message for the "revert" says that the warning is going to remain in this almost-invisible state "until there's a way to declare the set of wanted warnings, not inhibit the unwanted." And that is going to happen who-knows-when.

Lesson learned? If *even* SBCL is capable of indefinitely withholding a perfectly sane, righteous, and meaningful compilation warning in order to maintain backwards compatibility with user code that is *broken*, then it means something.

* *(Is it just me, or can I see a parallel to another system which attempts to introduce a perfectly sane, righteous, and meaningful compilation warning and then gets information that doing so would maintain backwards compatibility with user code?...)*

--------------------

The reality as I see it right now is that Common Lisp implementation developers and maintainers also happen to *release* their implementations. Incidentally, they tend to release ASDF along with their implementations, which causes them to bear the burden of tacking an external module onto their releases, and whenever that module upgrades - an even bigger burden of ensuring that upgrades of that module do not cause incompatibilities with user code.

That approach doesn't work well in practice, because:

* ASDF is currently more than capable of bearing the burden of supporting ASDF in all contemporary Lisp implementations - it has the people, the knowledge, the will, and the energy to do so,
* Implementers, on the other side, don't.

A way forward that I see is: lift the burden of being responsible for ASDF upgrades from implementers *at all costs*. That's the first step of solving the underlying true problem of lack of communication between ASDF-devel maintainers and real-world ASDF-stable users.

Well, it's not that there's little communication - it's even that ASDF-devel maintainers don't know the full extent of where ASDF is used, and therefore where support might be needed during upgrades. And that's a situation where breakages can happen silently and quietly, disincentivizing people from upgrading ASDF in the future.

The above is, IMO, the most core issue to be solved by ASDF in the long-term. Because of it, it's impossible to e.g. discover breakages that would normally be held internally, and to create upgrade paths for these users. The breakages can include some custom system definitions that ASDF-devel has no idea about, and therefore also has no idea if or how it is breaking them.

If the breakages are also not reported upstream but remain silently fixed by programmers on-site, that also causes frustration and grudges to build up - and, if they already exist, to build up further. And I guess we all know where that leads.

An `asdf-devel` mailing list is, IMO, a good start, but what's needed is an actual call-out to ASDF users that will be heard by these users - a message that implementations or implementers no longer need to support ASDF, that Team ASDF is capable (and willing!) to support it themselves.

After "how do we reach these users?" is solved (and I have no idea what is the best way of doing it), then what's left is the technical and legal problems of volunteering and/or contracting, such as signing up NDAs for access to private or sensitive code; I have no idea if CLF is capable of helping with that as a corporation. I suppose it could, but I haven't asked yet - so here I am, throwing an idea out.

Finally, ASDF will need data about its users in order to eventually re-merge ASDF-stable and ASDF-devel. (Which, I assume, that's the *real* outcome that many Lisp hackers actually want - nobody wants another long-lasting fork in the build system world. But more on that below.)

Finally², Phoebe and Eric asked me about the "systemic problem" I've mentioned in a Reddit comment [here](https://www.reddit.com/r/Common_Lisp/comments/s02yhz/stas_has_alienated_longtime_asdf_maintainer/hs1jg1n/) - I hope that it's now clearer what I meant by it. Summarizing: it's communication between the known and the yet unknown real-world users of ASDF-stable and maintainers of ASDF-devel, *without* the need for intermediaries or implementers.

Finally³, it can be noted that the above approach cherry-picks some aspects of the people-oriented world into the tool-oriented world - in particular, it implements and exercises some communication protocols between the different "tribes" that comprise the current Common Lisp ecosystem.

* *Another noteworthy thing is that people who perform that cherry-picking can choose which of those aspects are worth cherry-picking and which are not - and, as usual, that brings both power and responsibilities. (And a chance for conflicts as well - but I simply hope we can manage that one.)*

------------------

The previous part mentioned the practice of distributing ASDF with implementations as a conflict zone regading ASDF upgrades. I don't think it *needs* to be a conflict zone, or that ASDF actually *needs* to depend on implementations in any way.

My idea is to piggyback on top of Quicklisp. There's an obvious chicken-and-egg problem of needing ASDF to use Quicklisp, but that isn't a problem in practice - Quicklisp is already used to distribute *non-ASDF-managed code* - most notably, Slime, via [quicklisp-slime-helper](https://github.com/quicklisp/quicklisp-slime-helper).

The architecture of `q-s-h` makes it depend on the Quicklisp `swank` system, which includes Slime sources and is always versioned along with the Quicklisp dist. That makes it not feasible for distributing different versions of ASDF and being able to switch between them.

Still, the idea is to create an ASDF system that is capable of automatically downloading an ASDF release tarball (e.g. via [Drakma](https://github.com/edicl/drakma/)), unpack it a known location on the system (for instance `$XDG_CONFIG_HOME/quicklisp-asdf-helper/asdf/asdf-3.3.4.1.lisp`), compile it into a FASL for performance, and then create or edit an entry that loads the proper ASDF version in the implementation's RC file.

Example API could encompass e.g.
* `(quicklisp-asdf-helper:use-asdf "3.3.4.1")` for a concrete version,
* `(quicklisp-asdf-helper:use-asdf :newest-release)` for the newest release,
* `(quicklisp-asdf-helper:use-asdf :git-master)` for the bleeding edge.

Execute that, and just like with `quicklisp-slime-helper`, you're ensured to have an entry in your RC file that loads the requested version of ASDF, already downloaded and available on your hard disk. Such a system would also make it easier for people who are running CI to download and load arbitrary ASDF versions, and therefore be able to test against them.

* *In case I missed something, though, an open question that I'd like to get some answers for is: **why**, in practice, should any ASDF-devel developer need to care about the version of ASDF bundled with SBCL?*

----------

Let's make a second take on "who are the users of ASDF-devel" - this time, a personal one. (For the record, I'm a user of both SBCL and ASDF.)

I enjoy ASDF warnings in my own systems, since I can understand the attempt to maintain regularity in order to have performance. The approach where ASDF removes such a warning about a misnamed system would be a downgrade for me.

Similarly, the approach where SBCL removes warnings that are useful for me (e.g. the old but loved one about implicitly creating a generic function via `DEFMETHOD`, invalid initform types mentioned before) is also a downgrade for me - I no longer get useful information out of the system that I would have otherwise received for free. (More about these additional warnings later on.)

End of a personal take. Let's broaden the scope a little bit.

At the same time while I enjoy these warnings, there will be users with larger bases of legacy code, and they will want to have some ASDF warnings turned off, just like there are users (e.g. Quicklisp!) who want to have some SBCL warnings turned off.

I'm unsure of how exactly to treat that "silent majority". The easiest way of thinking would be, either they want to update or they don't; in case of updates, then they can try and take the cost of fixing their code, and in case of no updates, we don't need to care about them.

The issue is that upgrades aren't a binary problem. Some people might want to keep their SBCL version and upgrade only ASDF - what happens with them? Another group of people can attempt to keep their ASDF version pinned and upgrade only SBCL - at which point SBCL had, and still has, a similar problem on their hands with regard to defaults about warnings.

* *(At least, and thankfully, it is mostly possible to weed out the problem of users who want to keep their old code supported **and** to get new features at the same time; pinning their dependencies instead of using the latest release is always an option, even if it is not exercised often in the Common Lisp world.)*

-----------

The Common Lisp programming world is in many ways append-only *because many of Common Lisp programmers want it to be this way* and it is a force to be reckoned with. If some people want to merely specify the unspecified, then there is fertile ground for that; a bigger issue is to be able to amend past design mistakes and break existing code.

At that point, the approach of "let's have a culture where signaling many `STYLE-WARNING`s and `WARNING`s is a good and welcome thing" and the approach of "treat all `WARNING`s as `ERROR`s" are at odds, no matter if we're talking about SBCL or ASDF. That's a technical problem but also a social one, and the social norms regarding warnings both define and are defined by the technical settings for handling them.

But it is also a problem that I consider common, at least common enough to be present in ASDF and SBCL and other projects alike, and I therefore think it should have a common solution - even if the people from Team SBCL and Team ASDF tend to glare at one another every now and then.

-----------

A possible solution for the issue from the above paragraph is an idea that I've taken from discussions on `#sbcl` on Libera Chat - a mechanism that can be configured for signaling additional warnings that some users find useful and some users find unwanted.

This includes the SBCL warnings I've mentioned earlier - mistyped initforms, implicit `DEFGENERIC`, both `&KEY` and `&OPTIONAL` in a lambda list, et cetera. Things that I'd like to remember while working on new code and *possibly* want to check for in already existing code.

A possible implementation, sans technical difficulties, is setting or binding some dynamic variable exposed by SBCL. Something like `(setf sb-ext:*additional-compiler-notes* '(:slot-initform-type-error :implicit-defgeneric :both-&key-and-&optional))` in my RC file could do the trick for code that I develop, rebinding that variable around calls to `ql:quickload` and `asdf:load-system` will do it for code that I want to Just Build™, while always leaving the default value of `'()` will do it for existing users who don't want to change a thing.

* Technical detail: there's a reason why keywords are used there; can be keywords, gensyms, strings, just *not* symbols whose identity is important. In particular, symbols that name conditions are not reliable for a technical reason of not being *forwards* compatible - if a hypothetical version ASDF 3.6 introduces a new condition named `ASDF:BAD-STYLE-WARNING`, then it becomes impossible to use `(HANDLER-BIND ((ASDF:BAD-STYLE-WARNING ...)) ...)` on code that needs to work with earlier versions of ASDF - *because the symbol `ASDF:BAD-STYLE-WARNING` is not exported in these earlier versions*. And that means either dirty reader conditionals or duplicating code for each new version of ASDF that introduces a new symbol naming a condition type.
* Disclosure: I will want to help implementing such a mechanism in SBCL and possibly in ASDF, even if just from pure egoism - I like these warnings and I want to see them for new code that I'm writing.

My train of thought is, if SBCL is capable of delaying issuing actual warnings about mistyped initforms instead of breaking everything in terms of backwards compatibility, I think it's possible and feasible to take this approach with ASDF as well.

---------------

Regarding the above, there is the obvious and understandable worry of, "if the warnings are opt-in, who will use them?" and "if they're opt-in, who will even *know* about them?".

Despite that worry, I'm still optimistic. I think that if package-local nicknames were capable of proliferating, and therefore solving this issue, *despite* not being standardized and therefore described in CLHS, then such a set of warnings can do the same; in my eyes, the only thing more powerful than a standard is a *de-facto standard*, which such a set of warnings should become.

How to do that? For the free software CL world, I can imagine e.g. Quicklisp making "stable" builds which are meant to make everything Just Build™ - and then making "development" runs which have all the additional warnings turned on without breaking the build, in order to highlight all possible warnings, style warnings, and compiler notes that are highlightable. For the closed-source world, the same would need to be advertised by maintainers of respective software systems - SBCL, ASDF, et al.

Once such an artifact exists, maintainers of existing systems can take steps to analyze the warnings and decide if their code should be improved with regard to them. That's a possible and, IMO, feasible path forward to improving code quality at large over a longer period of time, since it highlights possible problems with code. And once enough time passes, it can be re-evaluated whether turning warnings on breaks any code that is *currently* in use.

---------

Another idea is automatic detection of whether a given ASDF installation was ever upgraded from a previous version or if it has been installed fresh. Maybe it's possible that ASDF can use that information to conditionalize the types of warnings that it signals - in case of upgrades, it might want to turn off some warnings as a default, whereas it might want to freely warn on installations that are discovered to be brand new.

Yet another idea is, quote, ["filling gaps in ASDF testing"](https://github.com/edicl/flexi-streams/issues/25#issuecomment-1008519149) proposed by Eric Timmons in a GitHub comment, especially in face of automatic CI that he seems to pursue - I'll leave that one without further comment, though; while I would like to help with communication in this particular aspect, I need to apologize - my brain is kind of fried at the moment after writing a lot of Markdown in a very short time window.

---------------

At least in my opinion, backwards compatibility in the Common Lisp world will be maintained in the longer term simply because many people in it want it. And by "will be maintained", I mean, "**will** be maintained" - by now it's not really a personal choice of anyone, it's much closer a force of nature. 

In particular, there will be no large-scale choice to *not* maintain it - until a larger, language-and-standard-library-scale fork happens, and a new Lisp dialect emerges from Common Lisp, taking the good things and removing all that it considers "cruft".

Obviously, as with all such grand endeavors, only history will show if it turns out to be a short-lived flicker or something that's capable of gaining long-term usage and ground.

Given my impression of the Common Lisp programming world and both tools and people who comprise it, I think that such an occurrence is inevitable, but I won't be the one to bear the personal responsibility of saying whether or when it will or should happen.

*(Oh. Don't mind me, just musings of a 30-year-old Lisp boomer.)*

--------------

# tl;dr

## the sad part

* the CL programming world ain't like those of other languages
* backwards compatibility is pretty damn important
* lots of software in the wild depends on default behavior that doesn't change
* Hyrum's law is a thing, that is also true in the CL world
* bla bla bla, lots of other lisp-related context you can likely skip
  * except for one hot take in the beginning
    * no you seriously gotta go back and read this one

## the non-sad part

* even SBCL breaks things sometimes
* even SBCL needs to not do the right thing™ sometimes
* so basically yanno
  * remove the burden of keeping up with ASDF releases from lisp implementation maintainers
  * let ASDF developers communicate more directly with ASDF users
  * bring back and keep backwards-compatible defaults for ASDF warnings
  * make it possible and feasible to turn on additional warnings in ASDF and SBCL
  * `QUICKLISP-ASDF-HELPER` needs to become a thing

--------------

*(Thanks for making it to the very end. If you like my writing, feel free to drop me a [line](mailto:phoe@disroot.org) or toss me a [coin](https://github.com/sponsors/phoe/).)*

*(Thanks to the many not-to-be-named-here Lispers who reviewed this post before publishing and provided valuable insight, information, and details that made it as complete as it is now.)*

*(Goodness, 66kB of Markdown.)*
