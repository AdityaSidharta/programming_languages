# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [
               [[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # Ts
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]),
               [[[0, 0], [1, 0], [-1, 0], [2, 0], [-2, 0]],
               [[0, 0], [0, 1], [0, -1], [0, 2], [0, -2]]],
               rotations([[0, 0], [1, 0], [0, 1]]),
               rotations([[0, 0], [1, 0], [0, 1], [-1, 0], [-1, -1]])
               ] 
  
  Cheat_Pieces = [[[[0,0]]]]

  # your enhancements here
  def self.next_piece (board)
    Piece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    Piece.new(Cheat_Pieces.sample, board)
  end
  

end

class MyBoard < Board

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def next_piece
    if @game.is_cheatmode?
      @current_block = MyPiece.cheat_piece(self)
      @game.disable_cheat
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.length() - 1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def reduce_score
    @score = @score - 100
  end

end

class MyTetris < Tetris
  # your enhancements here

  def initialize
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    @cheatmode = false
    key_bindings
    buttons
    run_game
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def is_cheatmode?
    @cheatmode
  end

  def disable_cheat
    @cheatmode = false
  end


  def cheat
    if not @cheatmode
      if @board.score > 100
        @cheatmode = true
        @board.reduce_score
      end
    end
  end

  def key_bindings
    @root.bind('n', proc {self.new_game})

    @root.bind('p', proc {self.pause})

    @root.bind('q', proc {exitProgram})

    @root.bind('a', proc {@board.move_left})
    @root.bind('Left', proc {@board.move_left})

    @root.bind('d', proc {@board.move_right})
    @root.bind('Right', proc {@board.move_right})

    @root.bind('s', proc {@board.rotate_clockwise})
    @root.bind('Down', proc {@board.rotate_clockwise})

    @root.bind('w', proc {@board.rotate_counter_clockwise})
    @root.bind('Up', proc {@board.rotate_counter_clockwise})

    @root.bind('u', proc {@board.rotate_180_degrees})

    @root.bind('space' , proc {@board.drop_all_the_way})

    @root.bind('c' , proc {self.cheat})
  end
end


