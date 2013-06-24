#-----------------------------------------------------------------------------
#  Emacs-like bindings for Tk text widgets
#
#	Andrew C. Payne
#	payne@crl.dec.com
#
#-----------------------------------------------------------------------------

set tk_priv(cutbuffer) ""

#-----------------------------------------------------------------------------
# Keyboard bindings, model after emacs
#-----------------------------------------------------------------------------

proc emacs-text-move {w where} {
	global tk_priv

	$w mark set insert $where
	$w yview -pickplace insert
	if {$tk_priv(selectMode) == "select"} {
		$w tag remove sel 0.0 end
		$w tag add sel anchor insert
	}
}

proc emacs-twiddle {w} {
	set c [$w get insert-1c]
	$w delete insert-1c
	$w insert insert-1c $c
}

proc emacs-move-page {w dir} {
	global tk_priv

        set height [lindex [$w configure -height] 4]
        $w mark set insert "insert $dir $height lines"
        $w yview -pickplace insert
	if {$tk_priv(selectMode) == "select"} {
		$w tag remove sel 0.0 end
		$w tag add sel anchor insert
	}
}

#
#  If there is a current selection, delete it.  Else, backspace one character
#
proc emacs-backspace {w} {
	if {[catch {$w delete sel.first sel.last}]} {
		tk_textBackspace $w
	}
	$w yview -pickplace insert	
}

bind Text <Any-KeyPress> {
	if {%k == 140} {
		set tk_priv(selectMode) {}
		catch {set tk_priv(cutbuffer) [%W get sel.first sel.last]}
		catch {%W delete sel.first sel.last}
	}
	if {"%A" != ""} {
		%W insert insert %A
	}
	%W yview -pickplace insert
}

# By default, all the control and meta keys are disabled
bind Text <Control-Key> {
    %W yview -pickplace insert
}
bind Text <Meta-Key> {
    %W yview -pickplace insert
}
bind Text <Control-Meta-Key> {
    %W yview -pickplace insert
}
bind Text <Escape> {
    %W yview -pickplace insert
}

set tk_last_deleted ""
bind Text <Control-k>	{
	global tk_last_deleted
	set tk_last_deleted [%W get insert {insert lineend}]
        %W delete insert {insert lineend}
}
bind Text <Control-y>   {
	global tk_last_deleted
	%W insert insert $tk_last_deleted
        %W yview -pickplace insert
}
 
bind Text <Up> 		{emacs-text-move %W insert-1l}
bind Text <Down> 	{emacs-text-move %W insert+1l}
bind Text <Left> 	{emacs-text-move %W insert-1c}
bind Text <Right> 	{emacs-text-move %W insert+1c}

bind Text <Control-a>	{emacs-text-move %W {insert linestart}}
bind Text <Control-b> 	{emacs-text-move %W insert-1c}
bind Text <Control-d>	{%W delete insert insert+1c}
bind Text <Control-e>	{emacs-text-move %W {insert lineend}}
bind Text <Control-f> 	{emacs-text-move %W insert+1c}
bind Text <Control-h>	{emacs-backspace %W}
bind Text <Control-n> 	{emacs-text-move %W insert+1l}
bind Text <Control-o>	{%W insert insert "\n"; emacs-text-move %W insert-1c}
bind Text <Control-p> 	{emacs-text-move %W insert-1l}
bind Text <Control-t>	{emacs-twiddle %W}
bind Text <Control-v>	{emacs-move-page %W +}

bind Text <Prior> 	{emacs-move-page %W -}
bind Text <Next>	{emacs-move-page %W +}
bind Text <Delete>	{emacs-backspace %W}

bind Text <Insert> {
        %W insert insert $tk_priv(cutbuffer)
        %W yview -pickplace insert
}

bind Text <Select> {
	%W tag remove sel 0.0 end
	if {$tk_priv(selectMode) == "select"} {
		set tk_priv(selectMode) {}
	} {
		%W mark set anchor insert
		set tk_priv(selectMode) select
	}
}


#-----------------------------------------------------------------------------
#  Mouse bindings
#-----------------------------------------------------------------------------

bind Text <1> {
	set tk_priv(selectMode) char
	%W mark set insert @%x,%y
	%W mark set anchor insert
	if {[lindex [%W config -state] 4] == "normal"} {focus %W}
	%W tag remove sel 0.0 end
}

#
#  Button 2 is used to paste the current X selection, just like many X
#  applications.  This is the default Motif binding.
#
bind Text <2> {
	catch {	
		%W insert insert [selection get]
		%W yview -pickplace insert
	}
}

bind Text <B2-Motion> {}

#
#  Use button 3 as a drag for window text (just like the old Tk button 2
#  binding.
#
bind Text <3>		{%W scan mark %y}
bind Text <B3-Motion>	{%W scan dragto %y}


#-----------------------------------------------------------------------------
#  Emacs-like bindings for Tk entry widgets
#-----------------------------------------------------------------------------

# By default, all the control and meta keys are disabled
bind Entry <Control-Key> {
    tk_entrySeeCaret %W
}
bind Entry <Meta-Key> {
    tk_entrySeeCaret %W
}
bind Entry <Control-Meta-Key> {
    tk_entrySeeCaret %W
}
bind Entry <Escape> {
    tk_entrySeeCaret %W
}

# need to repeat these because they've just been overwritten
bind Entry <Control-h> {tk_entryBackspace %W; tk_entrySeeCaret %W}
bind Entry <Control-u> {%W delete 0 end}
bind Entry <Control-v> {%W insert insert [selection get]; tk_entrySeeCaret %W}
bind Entry <Control-w> {tk_entryBackword %W; tk_entrySeeCaret %W}

# Some Emacs bindings
bind Entry <Control-a> {%W icursor 0; tk_entrySeeCaret %W}
bind Entry <Control-e> {%W icursor end; tk_entrySeeCaret %W}
bind Entry <Control-k> {%W delete insert end}
bind Entry <Control-d> {%W delete insert}

set entry_cursor_index ""
bind Entry <Control-f> {
    global entry_cursor_index
    set entry_cursor_index [expr {[%W index insert] + 1}]
    %W icursor $entry_cursor_index
}
bind Entry <Control-b> {
    global entry_cursor_index
    set entry_cursor_index [expr {[%W index insert] - 1}]
    %W icursor $entry_cursor_index
}

