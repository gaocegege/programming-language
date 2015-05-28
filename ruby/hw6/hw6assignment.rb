# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # your enhancements here
  All_My_Pieces = All_Pieces + 
               [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [1, 2]]), # 6
               [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # long5 (needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
               rotations([[0, 1], [1, 0], [1, 1]]) # small L
               ]

  Cheat_Piece = [[[0,0]]]

  def self.cheat_piece (board)
  	MyPiece.new(Cheat_Piece, board)
  end

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def next_piece
  	if @cheat
  		@current_block = MyPiece.cheat_piece(self)
  		@cheat = false
  	else
    	@current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
  	if @score >= 100 and !@cheat
  		@score -= 100
  		@cheat = true
        @game.update_score
  	end
  end

end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
    super 

    # u
    @root.bind('u', lambda { @board.rotate_clockwise
    	@board.rotate_clockwise })

    # c
    @root.bind('c', proc {@board.cheat})
  end
end


