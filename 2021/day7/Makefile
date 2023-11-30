# File names
TARGET		:= aoc
SRCS	    = $(wildcard ./*.cpp)
OBJS    	= $(SRCS:%.cpp=%.o)

# Compiler Vars and Flags
CC		:= gcc
CXX		:= g++
ERRFLAGS	:= -Wall -Wextra -Wsign-conversion
DBGFLAGS	:= -ggdb3 -Og -DDEBUG
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

.PHONY: clean
clean:
	-rm -f $(TARGET) $(OBJS)
