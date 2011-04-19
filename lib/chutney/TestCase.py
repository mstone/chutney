
class TestAtom:
    def __init__(self, parent):
        self._parent = parent

    def discover(self):
        return []

    def configure(self, sysconfig, params):
        raise NotImplemented()

    def go(self):
        raise NotImplemented()

ALL_STATES = set([
       "undected",
       "detected",
       "configured",
       "launched",
       "succeeded",
       "failed",
       "cantrun",
       "canceled"
 ])

TERMINAL_STATES = set([
       "succeeded",
       "failed",
       "cantrun",
       "canceled"
])


UNSUCCESSFUL_STATES = set([
       "failed",
       "cantrun",
       "canceled"
])

class TestWatcher:
    def _notify(self, sender, newstate, message):
        raise NotImplemented()

class TestState(TestWatcher):
    def __init__(self, parent, name):
        self._parent = parent
        self._state = "undetected"
        self._name = "%s.%s"%(parent.getName(), name)
        # This can get circular.  If we care, add weakrefs.
        self._children = []
        self._watchers = [ parent ]
        parent.addChild(self)

    def getName(self):
        return self._name

    def __str__(self):
        return self._name

    def __repr__(self):
        return "TestState(%s)"%self._name

    def getState(self):
        return self._state

    def _log(self, name, newstate, message):
        print "%s %s %r" %(name, newstate, message)

    def _transition(self, newstate, message=None):
        assert newstate in ALL_STATES
        assert newstate != self._state
        assert self._state not in TERMINAL_STATES
        self._log(self._name, newstate, message)
        self._state = newstate

        for testState in self._watchers:
            testState._notify(self, newstate, message)

    def say(self, message):
        self._log(self._name, self._state, message)

    def succeed(self, message=None):
        self._transition("succeeded", message)

    def fail(self, message):
        self._transition("failed", message)

    def addChild(self, child):
        self._children.append(child)

    def addWatcher(self, watcher):
        self._watchers.append(watcher)

    def _notify(self, sender, newstate, message):
        if sender is self._parent and newstate in TERMINAL_STATES:
            self._transition("canceled", "parent %s"%newstate)
            self.cancelWork()

    def cancelWork(self):
        pass

class GroupTestState(TestState):
    # Short-circuit version
    def __init__(self):
        TestState.__init__(self)
        self._kids = set()
        self._anyFailed = False

    def addChild(self, child):
        TestState.addChild(self, child)
        self._kids.add(child)
        child.addWatcher(self)

    def _notify(self, sender, newstate, message):
        TestState.notify(self, sender, newstate, message)

        if sender not in self._kids:
            return

        if newstate == 'failed' and self._state not in TERMINAL_STATES:
            self.fail("subtask %s failed", sender.getName())

        elif newstate in TERMINAL_STATES and self._state not in TERMINAL_STATES:
            del self._kids[sender]
            if not self._kids:
                self.succeed()

class PatientGroupTestState(TestState):
    # Non-short-circuit version
    def __init__(self):
        TestState.__init__(self)
        self._kids = {}
        self._nFinished = 0

    def addChild(self, child):
        TestState.addChild(self, child)
        self._kids[child] = child.getState()
        child.addWatcher(self)

    def _notify(self, sender, newstate, message):
        TestState.notify(self, sender, newstate, message)

        if sender not in self._kids:
            return

        self._kids[sender] = newstate
        if newstate in TERMINAL_STATES:
            self._nFinished += 1

        if self._nFinished = len(self._kids):
            badChildren = []
            for ch,s in self._kids.iteritems():
                assert s in TERMINAL_STATES
                if s == "failed":
                    badChildren.append(ch)
            if anyBad:
                self.fail(("subtasks failed", badChildren))
            else:
                self.succeed()


class FrobbedTestState(TestState):

    def assert_(self, property, msg=None):
        #XXXX attach a stack trace? etc
        if not property:
            self.fail(msg)
        else:
            self.say(msg + " ok")

    def assertEquals(self, v1, v2, msg):
        if v1 != v2:
            self.fail(msg)
        else:
            self.say("%r == %r"%(v1,v2))



# ------------------------------------------------------------

class Invariant(TestAtom):
    pass

class TestCase(TestAtom):
    pass

class Task(TestAtom):
    pass

class NotDead(Invariant):
    def __init__(self, parent):
        Invariant.__init__(self, parent)

        # make ourselves a master task state.


    def configure(self, sysconfig, params):
        self._sysconfig = sysconfig

        for node in self._sysconfig.getNodes():
            # make a task for the node, and add it to our task state, and
            # remember it someplace
            pass

    def go(self):

        for node, nodetasks in self._nodetasks:
            nodetask.run()

    def checkNode(
        
