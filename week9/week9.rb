class A
  def m1
    34
  end

  def m2 (x,y)
    z = 7
    if x > y
      false
    else
      x + y * z
    end
  end
end

class B
  def m1
    4
  end

  def m3 x
    x.abs * 2 + self.m1
  end
end

class C
  def m1
    print "hi"
    self
  end
  def m2
    print "bye"
    self
  end
  def m3
    print "\n"
    self
  end
end

class D
  def initialize(f=0)
    @foo = f
  end
  
  def m1
    @foo = 0
  end

  def m2 x
    @foo += x
  end

  def foo
    @foo
  end
end

class E
  Dans_Age = 38

  def self.reset_bar
    @@bar = 0
  end

  def initialize(f=0)
    @foo = f
  end

  def m2 x
    @foo += x
    @@bar += 1
  end

  def foo
    @foo
  end

  def bar
    @@bar
  end
end

class MyRational
  def initialize(num, den=1)
    if den == 0
      raise ""
    elsif den < 0
      @num = - num
      @den = - den
    else
      @num = num
      @den = den
    end
    reduce
  end

  def to_s
    ans = @num.to_s
    if @den != 1
      ans += "/" + @den.to_s
    end
    ans
  end
  
  def to_s2
    dens = ""
    dens = "/" + @den.to_s if @den != 1
    @num.to_s + dens
  end

  def to_s3
    "#{@num}#{if @den == 1 then "" else "/" + @den.to_s end}"
  end

  attr_reader :den, :num
  def add! r
    a = r.num
    b = r.den
    c = @num
    d = @den
    @num = (a * d) + (b * c)
    @den = b * d
    reduce
    self
  end

  def + r
    ans = MyRational.new(@num, @den)
    ans.add! r
    ans
  end
  private
  def gcd(x,y)
    if x == y
      x
    elsif x < y
      gcd(x, y-x)
    else
      gcd(y, x)
    end
  end

  protected
  def reduce
    if @num == 0
      @den = 1
    else
      d = gcd(@num.abs, @den)
      @num /= d
      @den /= d
    end
  end
  
end

class Foo
  def initialize(max)
    @max = max
  end

  def silly
    yield(4,5) + yield(@max, @max)
  end

  def count base
    if base > @max
      raise "reached max"
    elsif yield base
      1
    else
      1 + (count(base+1) {|i| yield i})
    end
  end
end

# subclassing
class Point
  attr_accessor :x, :y # define methods x, y, x=, y=

  def initialize(a, b)
    @x = a
    @y = b
  end
  def distFromOrigin
    Math.sqrt(@x * @x, @y * @y) # uses instances variables
  end
  def distFromOrigin2
    Math.sqrt(x * x, y * y) #uses getter methods
  end
end

class ColorPoint < Point
  attr_accessor :color #define methods color, color=

  def initialize(x,y,c="clear")
    super(x,y)
    @color = c
  end
end

class ThreeDPoint
  attr_accessor :z

  def initialize(x, y, z)
    super(x,y)
    @z = z
  end

  def distFromOrgin
    d = super
    Math.sqrt(d * d + @z * @z)
  end

  def distFromOrigin2
    d = super
    Math.sqrt(d * d + z * z)
  end
end

class PolarPoint < Point
  def initialize(r, theta)
    @r = r
    @theta = theta
  end

  def x
    @r * Math.cos(@theta)
  end
  def y
    @r * Math.sin(@theta)
  end

  def x= a
    b = y
    @theta = Math.atan2(b,a)
    @r = Math.sqrt(a*a + b*b)
    self
  end
  def y= b
    a = x
    @theta = Math.atan2(b, a)
    @r = Math.sqrt(a*a + b*b)
    self
  end
  def distFromOrigin # must override since inherited method does wrong thing
    @r
  end
  # inherited distFromOrigin2 already works!!
end

# call method n of instance of B1 will cause the program to run forever.
class A1
  def even x
    puts "in even A"
    if x==0 then true else odd(x-1) end
  end
  def odd x
    puts "in odd A"
    if x==0 then false else even(x-1) end
  end
end

class B1 < A1
  def even x
    puts "in even B"
    x % 2 == 0
  end
end

a2 = B1.new.odd 7 # the odd will call the function even of class B.

