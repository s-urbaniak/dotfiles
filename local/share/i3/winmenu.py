#!/usr/bin/env python
# dmenu script to jump to windows in i3.
#
# using ziberna's i3-py library: https://github.com/ziberna/i3-py
# depends: dmenu (vertical patch), i3.
# released by joepd under WTFPLv2-license:
# http://sam.zoy.org/wtfpl/COPYING
#
# edited by Jure Ziberna for i3-py's examples section

import i3
import subprocess
import pdb;

def i3clients():
    """
    Returns a dictionary of key-value pairs of a window text and window id.
    Each window text is of format "[workspace] window title (instance number)"
    """
    clients = {}
    for ws_num in range(1,11):
        workspace = i3.filter(num=ws_num)
        if not workspace:
            continue
        workspace = workspace[0]
        windows = i3.filter(workspace, nodes=[])
        instances = {}

        # Adds windows and their ids to the clients dictionary
        for window in windows:
            winprops = window.get("window_properties", {})
            cls = winprops.get("class", "empty")

            win_str = '[%s] %s "%s"' % (workspace['name'], cls, window['name'])
            # Appends an instance number if other instances are present
            if win_str in instances:
                instances[win_str] += 1
                win_str = '%s (%d)' % (win_str, instances[win_str])
            else:
                instances[win_str] = 1
            clients[win_str] = window['id']
    return clients

def win_menu(clients, l=10):
    """
    Displays a window menu using dmenu. Returns window id.
    """
    dmenu = subprocess.Popen(['/usr/bin/dmenu','-i','-l', str(l), '-sf', '#ffffff', '-sb', '#252936', '-nf', '#7780a1', '-nb', '#1f222d', '-fn', '"font pango: Bitstream Vera Sans"'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE)
    menu_str = '\n'.join(sorted(clients.keys()))
    # Popen.communicate returns a tuple stdout, stderr
    win_str = dmenu.communicate(menu_str.encode('utf-8'))[0].decode('utf-8').rstrip()
    return clients.get(win_str, None)

if __name__ == '__main__':
    clients = i3clients()
    win_id = win_menu(clients)
    if win_id:
        i3.focus(con_id=win_id)

