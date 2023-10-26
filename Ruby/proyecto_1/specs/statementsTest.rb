require 'minitest/autorun'
require_relative '../lib/parser.rb'

class StatementsTest < Minitest::Test

  # Assignment
  def test_assignment_unparse
    assignment = Assignment.new("x", Numeral.new(5))
    assert_equal "x = (5.0);", assignment.unparse
  end

  def test_assignment_evaluate
    assignment = Assignment.new("x", Numeral.new(5))
    state = {}
    assignment.evaluate(state)
    assert_equal 5, state["x"]
  end

  # Block
  def test_block_unparse
    block = Block.new([Assignment.new("x", Numeral.new(5)), Assignment.new("y", Numeral.new(3))])
    assert_equal "{ x = (5.0); y = (3.0); }", block.unparse
  end

  def test_block_evaluate
    block = Block.new([Assignment.new("x", Numeral.new(5)), Assignment.new("y", Numeral.new(3))])
    state = block.evaluate
    assert_equal 5, state["x"]
    assert_equal 3, state["y"]
  end

  # IfThenElse
  def test_if_then_else_unparse
    if_then_else = IfThenElse.new(ComparisonLessThan.new(Numeral.new(5), Numeral.new(3)),
                                  Assignment.new("x", Numeral.new(1)),
                                  Assignment.new("x", Numeral.new(2)))
    assert_equal "if (((5.0) < (3.0))) x = (1.0); else x = (2.0);", if_then_else.unparse
  end

  def test_if_then_else_evaluate_true
    if_then_else = IfThenElse.new(ComparisonLessThan.new(Numeral.new(5), Numeral.new(3)),
                                  Assignment.new("x", Numeral.new(1)),
                                  Assignment.new("x", Numeral.new(2)))
    state = if_then_else.evaluate
    assert_equal 2, state["x"]
  end

  def test_if_then_else_evaluate_false
    if_then_else = IfThenElse.new(ComparisonLessThan.new(Numeral.new(3), Numeral.new(5)),
                                  Assignment.new("x", Numeral.new(1)),
                                  Assignment.new("x", Numeral.new(2)))
    state = if_then_else.evaluate
    assert_equal 1.0, state["x"]
  end

  # WhileDo
  def test_while_do_unparse
    while_do = WhileDo.new(ComparisonLessThan.new(VariableExp.new("x"), Numeral.new(5)),
                           Assignment.new("x", Addition.new(VariableExp.new("x"), Numeral.new(1))))
    assert_equal "while (((x) < (5.0))) x = ((x) + (1.0));", while_do.unparse
  end

  def test_while_do_evaluate
    while_do = WhileDo.new(ComparisonLessThan.new(VariableExp.new("x"), Numeral.new(5)),
                           Assignment.new("x", Addition.new(VariableExp.new("x"), Numeral.new(1))))
    state = {"x" => 1}
    state = while_do.evaluate(state)
    assert_equal 5, state["x"]
  end

end
