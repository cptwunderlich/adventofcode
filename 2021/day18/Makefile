# File names
TARGET		:= aoc
SRCS	    = $(wildcard ./day*.cpp)
OBJS    	= $(SRCS:%.cpp=%.o)

# Compiler Vars and Flags
CC		:= gcc
CXX		:= g++
ERRFLAGS	:= -Wall -Wextra -Wsign-conversion
DBGFLAGS	:= -ggdb3 -O0 -DDEBUG
RELFLAGS	:= -O3 -DNDEBUG
CXXFLAGS	:= -std=c++17 $(ERRFLAGS)

.PHONY: all
all: CXXFLAGS += $(DBGFLAGS)
all: $(TARGET)

.PHONY: release
release: CXXFLAGS += $(RELFLAGS)
release: $(TARGET)

$(TARGET): $(SRCS)
	$(CXX) $(CXXFLAGS) $(SRCS) -o $@

tests: CXXFLAGS += $(patsubst -O%,-Og,$(DBGFLAGS))
tests: CXXFLAGS += -DCATCH_CONFIG_MAIN
tests: tests.cpp $(SRCS)
	$(CXX) $(CXXFLAGS) tests.cpp $(SRCS) -o $@

.PHONY: clean
clean:
	-rm -f tests $(TARGET) $(OBJS)
