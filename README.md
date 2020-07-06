# hs-todo
A minimalist TUI for the todo.txt format.

![screenshot](https://raw.githubusercontent.com/MaxMutantMayer/hs-todo/main/res/screenshot.png)

## Features
Currently only minimal functionality is given. More features will be added gradually (see res/example.txt for more information).
* Vim-inspired keybindings
* Add, delete and edit todo.txt entries
* Sort entries

## Build
Build on Unix systems with either nix or cabal:

### nix
```
nix-build
```
### cabal
```
cabal build
```

## Usage
```
hs-todo /path/to/todo.txt
```

## Keybindings
### List
<table>

<colgroup>
<col style="text-align:center;"/>
<col style="text-align:center;"/>
<col style="text-align:left;"/>
</colgroup>

<thead>
<tr>
	<th style="text-align:center;">Key</th>
	<th style="text-align:center;">Alternative</th>
	<th style="text-align:left;">Function</th>
</tr>
</thead>

<tbody>
<tr>
	<td style="text-align:center;">q</td>
	<td style="text-align:center;">Esc</td>
	<td style="text-align:left;">Exit hs-todo</td>
</tr>

<tr>
	<td style="text-align:center;">w</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Save file</td>
</tr>

<tr>
	<td style="text-align:center;">k</td>
	<td style="text-align:center;">up arrow key</td>
	<td style="text-align:left;">Up</td>
</tr>

<tr>
	<td style="text-align:center;">j</td>
	<td style="text-align:center;">down arrow key</td>
	<td style="text-align:left;">Down</td>
</tr>

<tr>
	<td style="text-align:center;">Ctrl-b</td>
	<td style="text-align:center;">PgUp</td>
	<td style="text-align:left;">Page Up</td>
</tr>

<tr>
	<td style="text-align:center;">Ctrl-f</td>
	<td style="text-align:center;">PgDown</td>
	<td style="text-align:left;">Page Down</td>
</tr>

<tr>
	<td style="text-align:center;">Ctrl-u</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Half Page Up</td>
</tr>

<tr>
	<td style="text-align:center;">Ctrl-d</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Half Page Down</td>
</tr>

<tr>
	<td style="text-align:center;">g</td>
	<td style="text-align:center;">Home</td>
	<td style="text-align:left;">Go to first element</td>
</tr>

<tr>
	<td style="text-align:center;">G</td>
	<td style="text-align:center;">End</td>
	<td style="text-align:left;">Go to last element</td>
</tr>

<tr>
	<td style="text-align:center;">Space</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Toggle done</td>
</tr>

<tr>
	<td style="text-align:center;">n</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Add new</td>
</tr>

<tr>
	<td style="text-align:center;">x</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Delete</td>
</tr>

<tr>
	<td style="text-align:center;">e</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Edit</td>
</tr>

<tr>
	<td style="text-align:center;">s</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Sort (as whole string)</td>
</tr>

<tr>
	<td style="text-align:center;">1</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Sort by done-checkmark</td>
</tr>

<tr>
	<td style="text-align:center;">2</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Sort by date</td>
</tr>

<tr>
	<td style="text-align:center;">3</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Sort by priority</td>
</tr>

<tr>
	<td style="text-align:center;">4</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Sort by projects</td>
</tr>

<tr>
	<td style="text-align:center;">5</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Sort by contexts</td>
</tr>

<tr>
	<td style="text-align:center;">=</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Revert sort</td>
</tr>

</tbody>
</table>

### Editor
<table>

<colgroup>
<col style="text-align:center;"/>
<col style="text-align:center;"/>
<col style="text-align:left;"/>
</colgroup>

<thead>
<tr>
	<th style="text-align:center;">Key</th>
	<th style="text-align:center;">Alternative</th>
	<th style="text-align:left;">Function</th>
</tr>
</thead>

<tbody>

<tr>
	<td style="text-align:center;">Esc</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Cancel</td>
</tr>

<tr>
	<td style="text-align:center;">Enter</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Confirm</td>
</tr>

<tr>
	<td style="text-align:center;">left arrow key</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Move left</td>
</tr>

<tr>
	<td style="text-align:center;">right arrow key</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Move right</td>
</tr>

<tr>
	<td style="text-align:center;">Ctrl-a</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Go to beginning of line</td>
</tr>

<tr>
	<td style="text-align:center;">Ctrl-e</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Go to end of line</td>
</tr>

<tr>
	<td style="text-align:center;">Ctrl-d</td>
	<td style="text-align:center;">Del</td>
	<td style="text-align:left;">Delete character at cursor</td>
</tr>

<tr>
	<td style="text-align:center;">Backspace</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Delete character prior cursor</td>
</tr>

<tr>
	<td style="text-align:center;">Ctrl-u</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Delete from cursor to beginning of line</td>
</tr>

<tr>
	<td style="text-align:center;">Ctrl-k</td>
	<td style="text-align:center;"></td>
	<td style="text-align:left;">Delete from cursor to end of line</td>
</tr>

</tbody>
</table>
