require 'minitest/autorun'
require_relative '../lib/parser.rb'

class ExpressionsTest < Minitest::Test

  # Numeral
  def test_numeral_unparse
    numeral = Numeral.new(5)
    assert_equal "(5.0)", numeral.unparse
  end

  def test_numeral_evaluate
    numeral = Numeral.new(5)
    state = {}
    assert_equal 5, numeral.evaluate(state)
  end

  # Minus
  def test_minus_unparse
    minus = Minus.new(Numeral.new(3))
    assert_equal "(-(3.0))", minus.unparse
  end

  def test_minus_evaluate
    minus = Minus.new(Numeral.new(3))
    state = {}
    assert_equal -3, minus.evaluate(state)
  end

  # Addition
  def test_addition_unparse
    addition = Addition.new(Numeral.new(2), Numeral.new(3))
    assert_equal "((2.0) + (3.0))", addition.unparse
  end

  def test_addition_evaluate
    addition = Addition.new(Numeral.new(2), Numeral.new(3))
    state = {}
    assert_equal 5, addition.evaluate(state)
  end

  # Subtraction
  def test_subtraction_unparse
    subtraction = Subtraction.new(Numeral.new(5), Numeral.new(3))
    assert_equal "((5.0) - (3.0))", subtraction.unparse
  end

  def test_subtraction_evaluate
    subtraction = Subtraction.new(Numeral.new(5), Numeral.new(3))
    state = {}
    assert_equal 2, subtraction.evaluate(state)
  end

  # Multiplication
  def test_multiplication_unparse
    multiplication = Multiplication.new(Numeral.new(2), Numeral.new(3))
    assert_equal "((2.0) * (3.0))", multiplication.unparse
  end

  def test_multiplication_evaluate
    multiplication = Multiplication.new(Numeral.new(2), Numeral.new(3))
    state = {}
    assert_equal 6, multiplication.evaluate(state)
  end

  # Division
  def test_division_unparse
    division = Division.new(Numeral.new(6), Numeral.new(2))
    assert_equal "((6.0) / (2.0))", division.unparse
  end

  def test_division_evaluate
    division = Division.new(Numeral.new(6), Numeral.new(2))
    state = {}
    assert_equal 3, division.evaluate(state)
  end
end
