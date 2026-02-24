CXX = g++
CXXFLAGS = -O3 -I/home/saul/Documents/eigen-5.0.0 -std=c++11 -fopenmp
LDFLAGS = -lm -fopenmp

# Targets
laplace: tridiagonal.cpp
	$(CXX) $(CXXFLAGS) -o laplace tridiagonal.cpp $(LDFLAGS)

clean:
	rm -f laplace neumann