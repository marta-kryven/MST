# edit an existing map, or create a new one using a grid-editing dialog
import tkinter as tk
from tkinter import filedialog

cell_width = 50

color_to_map = {"red" : "exit", "white": "open floor", "brown": "wall", "yellow": "start", "black": "hidden"}

class MapEditor:
    def __init__(self, root, rows = 10, cols = 10, maze = None):
        self.root = root
        self.rows = rows
        self.cols = cols
        self.colors = ['white', 'black', 'brown', 'yellow', 'red']
        self.current_color = self.colors[1]
        
        if (maze == None):
            self.grid = [[0 for _ in range(cols)] for _ in range(rows)]
        else:
            self.grid = [[maze[r][c] for c in range(cols)] for r in range(rows)]

        #print(self.grid, rows, cols)
        
        self.canvas = tk.Canvas(root, width=cols*cell_width, height=rows*cell_width, bg='white')
        self.canvas.pack()
        
        self.canvas.bind('<Button-1>', self.change_color)
        
        self.color_buttons = []
        for i, color in enumerate(self.colors):
            btn = tk.Button(root, bg=color, width=10, height=2, text=color_to_map[color], command=lambda c=color, i=i: self.set_color(i))
            btn.pack(side=tk.LEFT)
            self.color_buttons.append(btn)
        
        save_button = tk.Button(root, text="Save Map", command=self.save_bitmap)
        save_button.pack(side=tk.LEFT)
        
        self.draw_grid()
    
    def int_to_color_idx(self, color):
        if color == 6:
            return self.colors.index('white')
        if color == 0:
            return self.colors.index('black')
        if color == 3:
            return self.colors.index('brown')
        if color == 5:
            return self.colors.index('yellow')
        if color == 2:
            return self.colors.index('red')
        
    def draw_grid(self):
        for r in range(self.rows):
            for c in range(self.cols):
                x1, y1 = c*cell_width, r*cell_width
                x2, y2 = x1 + cell_width, y1 + cell_width
                self.canvas.create_rectangle(x1, y1, x2, y2, outline='black', fill=self.colors[ self.int_to_color_idx(self.grid[r][c])])
    
    def colour_to_int(self, colour):
        if colour == 'white':
            return 6
        if colour == 'black':
            return 0
        if colour == 'brown':
            return 3
        if colour == 'yellow':
            return 5
        if colour == 'red':
            return 2
        
    def change_color(self, event):
        col = event.x // cell_width
        row = event.y // cell_width
        if 0 <= row < self.rows and 0 <= col < self.cols:
            self.grid[row][col] = self.colour_to_int(self.current_color)
            self.draw_grid()
    
    def set_color(self, color_index):
        self.current_color = self.colors[color_index]
    
    def save_bitmap(self):
        file_path = filedialog.asksaveasfilename(defaultextension=".txt",
                                                 filetypes=[("Text files", "*.txt"), ("All files", "*.*")])
        if file_path:
            with open(file_path, 'w') as f:
                f.write(str(self.cols) + "\n")
                f.write(str(self.rows) + "\n")
                for row in self.grid:
                    f.write("".join(map(str, row)) + "\n")

def main():
    def read_maze(maze_name, exp):

        filename = '/Users/mk/Desktop/test.txt' # f'__experiment_{exp}/mazes/{maze_name}.txt'

        with open(filename, 'r') as f:
            ncols = int(f.readline())
            nrows = int(f.readline())
            maze = f.readlines()
    
        maze = tuple([tuple([int(cell) for cell in row.split('\n')[0]]) for row in maze])

        exit_pos = None

        for r,row in enumerate(maze):
            for c,cell in enumerate(row):
                if cell == 2:
                    exit_pos = (r,c)
    
        return maze, exit_pos

    # open an existing file to edit
    m, ep = read_maze = read_maze('4vs1', 1)
    w = len(m[0])
    h = len(m)

    root = tk.Tk()
    root.title("Map Editor")
    app = MapEditor(root, 10, 10)  #h, w, m) # or create new
    root.mainloop()

if __name__ == "__main__":
    main()
