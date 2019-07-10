import logging

import buffers
import core_interface
import minibuffer

from PyQt5.QtCore import QCoreApplication, QEvent, Qt, pyqtSlot
from PyQt5.QtGui import QKeyEvent, QKeySequence
from PyQt5.QtWidgets import QApplication, QShortcut, QVBoxLayout, QWidget


#: A dictionary of current windows mapping an identifier (str) to a window (Window).
WINDOWS = {}

#: Key modifiers.
MODIFIERS = {
    Qt.Key_Shift: "s",
    Qt.Key_Control: "C",
    Qt.Key_CapsLock: "Lock",
    Qt.Key_Alt: "M",
    Qt.Key_AltGr: "M",
    Qt.Key_Meta: "Meta",
    Qt.Key_Super_L: "S",
    Qt.Key_Super_R: "S",
    Qt.Key_Hyper_L: "H",
    Qt.Key_Hyper_R: "H",
}

#: Build modifiers back for Qt.
QT_MODIFIERS = {
    'C': Qt.KeyboardModifier.ControlModifier,
    'H': -1,  # TODO:
    'Lock': -1,  # TODO:
    'M': Qt.KeyboardModifier.AltModifier,
    'Meta': Qt.KeyboardModifier.MetaModifier,
    's': Qt.KeyboardModifier.ShiftModifier,
    'S': -1,  # TODO:
}

#: Special keys, to be understood by the lisp core.
KEY_TRANSLATIONS = {
    Qt.Key_Backspace: "BACKSPACE",
    Qt.Key_Space: "SPACE",
    Qt.Key_Delete: "DELETE",
    Qt.Key_Escape: "ESCAPE",
    Qt.Key_Return: "RETURN",
    Qt.Key_Tab: "TAB",
    # Qt.Key_Delete: "HYPHEN", # TODO:

    Qt.Key_Right: "Right",
    Qt.Key_Left: "Left",
    Qt.Key_Up: "Up",
    Qt.Key_Down: "Down",
}

GENERATED_KEYPRESS = False

CORE_INTERFACE = "engineer.atlas.next.core"
CORE_OBJECT_PATH = "/engineer/atlas/next/core"


def get_window(identifier):
    window = WINDOWS.get(identifier)
    if window:
        return window
    else:
        raise Exception("Window ID: {} not found!".format(identifier))


def is_modifier(key):
    return key in MODIFIERS.keys()


def is_special(key):
    return key in KEY_TRANSLATIONS.keys()


def key_to_special(inputkey):
    for key in KEY_TRANSLATIONS.items():
        if key[1] == inputkey:
            return key[0]


def build_qt_modifiers(names):
    """
    Given a list of strings designing modifiers names ("M", "C"),
    return a KeyboardModifiers class with the internal Qt representation of modifiers.
    If no result, return Qt.NoModifier (i.e, 0) instead.
    """
    res = []
    names = [it for it in names if it != ""]
    for name in names:
        mod = QT_MODIFIERS.get(name)
        if mod is None:
            logging.warn("Unrecognized modifier: {}".format(name))
        elif mod == -1:
            logging.warn("Unsupported modifier: {}".format(name))
            return Qt.NoModifier
        else:
            res.append(mod)
    if res:
        # TODO: doesn't work with a list of modifiers.
        # qt_modifiers = Qt.KeyboardModifiers(*res)
        qt_modifiers = Qt.KeyboardModifiers(res[0])
    else:
        qt_modifiers = Qt.NoModifier
    return qt_modifiers


def build_special_list(names):
    res = []
    for name in names:
        special = key_to_special(name)
        if special:
            res.append(special)
    return res


class KeyCaptureWidget(QWidget):
    """
    Subclass QWidget to catch key presses.
    """
    #: Record consecutive modifier keys.
    modifiers_stack = []
    #: Current event (key and all). We send all input event to the core.
    current_event = None

    #: Identifier (string) of the parent window.
    # XXX: this clearly is sub-optimal: Window.identifier,
    # window.qtwidget being KeyCaptureWidget, KeyCaptureWidget.parent_identifier being window.identifier
    parent_identifier = ""

    def __init__(self, identifier="<no id>"):
        self.parent_identifier = identifier
        super().__init__()

        # Without shortcuts, we cannot see these events on key press
        # (but only on key release).
        # They are (probably) caught by the webview, but overriding
        # the keyPressEvent method there didn't help.
        # Setting up a Qt filter didn't help either.
        QShortcut(QKeySequence("Ctrl+C"), self, activated=self.on_Ctrl_C)
        QShortcut(QKeySequence("Ctrl+X"), self, activated=self.on_Ctrl_X)
        QShortcut(QKeySequence("Escape"), self, activated=self.on_Escape)
        QShortcut(QKeySequence("Backspace"), self, activated=self.on_Backspace)

    @pyqtSlot()
    def on_Ctrl_C(self):
        logging.info("Ctrl+C")
        # XXX: this global ain't pretty.
        global GENERATED_KEYPRESS
        if GENERATED_KEYPRESS:
            GENERATED_KEYPRESS = False
            logging.info("----- got a generated C-c: give it to the app (TODO)")
            return
        self.quick_push_input(67, "c", ["C"])

    @pyqtSlot()
    def on_Ctrl_X(self):
        logging.info("Ctrl+X")
        global GENERATED_KEYPRESS
        if GENERATED_KEYPRESS:
            GENERATED_KEYPRESS = False
            logging.info("----- got a generated C-x: give it to the app (TODO)")
            return
        self.quick_push_input(88, "x", ["C"])

    @pyqtSlot()
    def on_Escape(self):
        logging.info("Escape")
        global GENERATED_KEYPRESS
        if GENERATED_KEYPRESS:
            GENERATED_KEYPRESS = False
            logging.info("----- got a generated ESC: give it to the app (TODO)")
            return
        self.quick_push_input(16777216, "ESCAPE", [""])

    @pyqtSlot()
    def on_Backspace(self):
        logging.info("Backspace")
        global GENERATED_KEYPRESS
        if GENERATED_KEYPRESS:
            GENERATED_KEYPRESS = False
            logging.info("----- got a generated BACKSPACE: give it to the app (TODO)")
            return
        self.quick_push_input(16777219, "BACKSPACE", [""])

    def quick_push_input(self, key_code, key_string, modifiers,
                         low_level_data=None, x=-1.0, y=-1.0, sender=None):
        # type signature: int, str, array of strings, double, double, int, str
        core_interface.push_input_event(
            key_code, key_string, modifiers,
            x, y,
            low_level_data if low_level_data else key_code,
            sender if sender else self.parent_identifier)

    def keyPressEvent(self, event):
        key = event.key()
        logging.info("key press: {}".format(key))
        if is_modifier(key):
            logging.info("------ got a modifier")
            self.modifiers_stack.append(key)
            # It's ok Qt, we handled it, don't handle it.
            # return True
            logging.info("Modifiers: {}".format(self.modifiers_stack))
        else:
            self.current_event = event
            key_code = event.key()
            key_string = event.text()

            if is_special(key_code):
                key_string = KEY_TRANSLATIONS[key_code]
                logging.info("this key is special: {} -> {}".format(key_code, key_string))
            else:
                # In case of C-a, text() is "^A", a non-printable character, not what we want.
                # XXX: lower() is necessary, but is it harmless ?
                try:
                    key_string = chr(key_code).lower()  # ascii -> string.
                except Exception:
                    # watch out arrow keys.
                    pass
                logging.debug("our new key_string: from {} to {}".format(event.text(), key_string))

            global GENERATED_KEYPRESS
            if GENERATED_KEYPRESS:
                GENERATED_KEYPRESS = False
                logging.info("----- generated keypress: return")
                return

            logging.info("Sending push-input-event with key_code {}, key_string {} and modifiers {}".format(
                key_code, key_string, self.get_modifiers_list()))
            self.quick_push_input(key_code, key_string, self.get_modifiers_list())



    def keyReleaseEvent(self, event):
        """
        Send all input events with a list of modifier keys.
        """
        # send R modifier to not double ?
        # Pierre: send the key release event too ? (not used as for now)
        # We want the list of modifiers when C-S-a is typed.  We might
        # have it with Qt.Keyboardmodifier() or event.modifiers() but
        # they don't return useful values so far. This works.
        key_code = event.key()
        logging.info("key release: {}".format(key_code))
        if is_modifier(key_code):
            self.modifiers_stack = []
            self.current_event = None
            # return True  # fails

    def get_modifiers_list(self):
        """
        Return the sequence modifiers as a list of strings ("C", "S" etc).
        """
        # dbus always expects an array of strings, not just [].
        # He doesn't know how to encode "None".
        # The lisp core removes empty strings before proceeding.
        return [MODIFIERS[key] for key in self.modifiers_stack] or [""]

    def get_key_sequence(self):
        """Return a string representing the sequence: keycode, keyval,
        list of modifiers, low level data, window id.
        """
        if self.current_event:
            out = "{}, {}, {}, {}, {}".format(
                "keycode",
                self.current_event.key(),
                self.get_modifiers_list(),
                "low level data",
                "window id",
            )
            logging.info("Key sequence: {}".format(out))

    def mouseDoubleClickEvent(self, event):
        logging.info("Double click")

    def mouseReleaseEvent(self, event):
        logging.info("Mouse release")

    def mousePressEvent(self, event):
        logging.info("Mouse press")


#: A window contains a window widget, a layout, an id (int), a minibuffer.
class Window():
    #: the actual QWidget.
    qtwindow = None
    #: layout, that holds the buffer and the minibuffer.
    layout = None
    #: window identifier (str)
    identifier = "0"
    #: the buffer
    buffer = None
    #: buffer height (px)
    buffer_height = 480
    #: the minibuffer is an object
    minibuffer = None
    #: minibuffer height (px)
    minibuffer_height = 20

    def __init__(self, identifier=None):
        self.qtwindow = KeyCaptureWidget(identifier=identifier)

        self.layout = QVBoxLayout()
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.identifier = identifier

        self.buffer = buffers.Buffer()
        self.buffer.set_height(self.buffer_height)
        self.minibuffer = minibuffer.Minibuffer(identifier)
        self.minibuffer.set_height(self.minibuffer_height)

        self.layout.addWidget(self.buffer.view)
        self.layout.addWidget(self.minibuffer.view)
        self.qtwindow.setLayout(self.layout)
        self.qtwindow.show()

    def set_title(self, title):
        """
        Set the title of the window.
        """
        logging.info("Set title: {}, {}".format(self.identifier, title))
        self.qtwindow.setWindowTitle(title)
        logging.info("Title set for window {} !".format(self.identifier))
        return title

    def set_active_buffer(self, buffer):
        """
        Set the active buffer of the window to buffer.
        """
        # Remove the current buffer from the layout (hide it).
        self.buffer.view.setParent(None)
        self.layout.insertWidget(0, buffer.view)
        self.buffer = buffer
        return True

    def set_minibuffer_height(self, height):
        assert isinstance(height, int)
        self.minibuffer.set_height(height)
        return True

    def minibuffer_evaluate_javascript(self, script):
        self.minibuffer.evaluate_javascript(script)
        return "0"  # TODO: callback ID

    def delete(self):
        # del self.qtwindow
        self.qtwindow.hide()
        return True

    def exists(self):
        if self.qtwindow.isVisible():
            return True


def make(identifier: str):
    """Create a window, assign it the given unique identifier (str).

    We must pass a reference to the lisp core's dbus proxy, in order
    to send asynchronous input events (key, mouse etc).

    return: The Window identifier
    """
    assert isinstance(identifier, str)
    window = Window(identifier=identifier)
    WINDOWS[window.identifier] = window
    logging.info("New window created, id {}".format(window.identifier))
    return identifier


def active():
    """
    Return the active window.
    """
    active_window = QApplication.activeWindow()
    logging.info("Active window by PyQt is {}.".format(active_window))
    for key, value in WINDOWS.items():
        if value.qtwindow == active_window:
            logging.info("Active window id: {}".format(value.identifier))
            return value.identifier
    logging.info("No active window found in {} windows.".format(len(WINDOWS)))


def generate_input_event(window_id, key_code, modifiers, low_level_data, x, y):
    """The lisp core tells us to generate this key event.

    - window_id: str
    - key_code: int
    - modifiers: [str]
    - low_level_data: key code from Qt (int).
    - x, y: float
    """
    qt_modifiers = build_qt_modifiers(modifiers)
    logging.info('generating this input event: window id {}, key code {}, modifiers names {}, \
    modifiers list {}'. format(window_id, key_code, modifiers, qt_modifiers))
    global GENERATED_KEYPRESS
    GENERATED_KEYPRESS = True
    # XXX: add a property to event, don't use globals. See window_key_event.
    event = QKeyEvent(QEvent.KeyPress, key_code, qt_modifiers)
    QCoreApplication.postEvent(get_window(window_id).qtwindow, event)
