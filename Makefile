CXX := clang++
BASE_CXXFLAGS := -std=c++17 -Wall -Wextra -O0 -g -Ithird_party/fmt/include -Ithird_party/expected/include 

# get llvm cxxflags and ldflags separately
LLVM_CXXFLAGS := $(shell llvm-config --cxxflags)
LLVM_LDFLAGS := $(shell llvm-config --ldflags --libs --system-libs core)

CXXFLAGS := $(BASE_CXXFLAGS) $(LLVM_CXXFLAGS) 
LDFLAGS := $(LLVM_LDFLAGS)

SRC_DIR := src
BUILD_DIR := build
TARGET := mccomp

SRCS := $(shell find $(SRC_DIR) -name "*.cpp")
OBJS := $(patsubst $(SRC_DIR)/%.cpp,$(BUILD_DIR)/%.o,$(SRCS))

all: $(TARGET)

$(TARGET): $(OBJS)
	@mkdir -p $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) $(OBJS) -o $(TARGET) $(LDFLAGS)
	cp $(TARGET) $(BUILD_DIR)/

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -I$(SRC_DIR) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR) $(TARGET)

.PHONY: all clean

