

# Create some example data
x <- seq(0, 10, length.out = 100)
y <- sin(x)

# Path to save inside the virtual environment
save_path <- "/mnt/data/sine_wave.png"

png(filename = save_path, width = 800, height = 600)
x <- seq(0, 10, length.out = 100)
y <- sin(x)
plot(x, y, type = "l", main = "Sine Wave", xlab = "X", ylab = "sin(X)")
dev.off()

# Output the save path
save_path
