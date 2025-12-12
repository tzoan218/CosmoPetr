package com.cosmo.backend.service;

import com.cosmo.backend.dto.InitialConditionsDTO;
import org.springframework.stereotype.Service;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

/**
 * Service to execute Fortran program with initial conditions
 */
@Service
public class FortranExecutionService {
    
    private static final Logger logger = LoggerFactory.getLogger(FortranExecutionService.class);
    
    /**
     * Maximum execution time in seconds (10 minutes)
     */
    private static final long MAX_EXECUTION_TIME = 600;
    
    /**
     * Get the absolute path to the Fortran executable
     */
    private Path getFortranExecutablePath() {
        // Get current working directory (usually backend/ when running Spring Boot)
        Path currentDir = Paths.get("").toAbsolutePath();
        logger.info("Current working directory: {}", currentDir);
        
        // Try multiple possible locations
        List<Path> possiblePaths = new ArrayList<>();
        
        // 1. Relative to current dir: ../spare/m.exe (if running from backend/)
        possiblePaths.add(currentDir.resolve("../spare/m.exe").normalize());
        
        // 2. Relative to current dir: ./spare/m.exe (if spare is in backend/)
        possiblePaths.add(currentDir.resolve("spare/m.exe"));
        
        // 3. Absolute path from project root (if we can detect it)
        // If current dir ends with "backend", go up one level
        if (currentDir.toString().endsWith("backend")) {
            possiblePaths.add(currentDir.getParent().resolve("spare/m.exe"));
        }
        
        // Find the first path that exists
        for (Path path : possiblePaths) {
            logger.info("Checking path: {}", path);
            if (Files.exists(path)) {
                logger.info("Found executable at: {}", path);
                return path;
            }
        }
        
        // Return the first path even if it doesn't exist (for error message)
        return possiblePaths.get(0);
    }
    
    /**
     * Execute Fortran program with given initial conditions
     * 
     * @param initialConditions Initial conditions from frontend
     * @return Execution result containing output files and status
     */
    public FortranExecutionResult executeFortran(InitialConditionsDTO initialConditions) {
        String executionId = UUID.randomUUID().toString();
        
        // Get the executable path first to determine the work directory
        Path executablePath = getFortranExecutablePath();
        Path workDir = executablePath.getParent(); // spare folder
        
        Path inputFile = workDir.resolve("input_" + executionId + ".txt");
        Path outputDir = workDir.resolve("output_" + executionId);
        
        logger.info("Starting Fortran execution with ID: {}", executionId);
        logger.info("Work directory: {}", workDir);
        logger.info("Input file: {}", inputFile);
        logger.info("Output directory: {}", outputDir);
        
        try {
            // Check if work directory exists
            if (!Files.exists(workDir)) {
                String errorMsg = "Work directory does not exist: " + workDir;
                logger.error(errorMsg);
                return new FortranExecutionResult(
                    executionId,
                    false,
                    errorMsg,
                    null,
                    null
                );
            }
            
            // Check if Fortran executable exists (already got it above)
            logger.info("Looking for executable at: {}", executablePath);
            
            if (!Files.exists(executablePath)) {
                String errorMsg = "Fortran executable not found at: " + executablePath + 
                    ". Please compile the Fortran code first: cd spare && gfortran gravitationalwaves.f -o m.exe";
                logger.error(errorMsg);
                return new FortranExecutionResult(
                    executionId,
                    false,
                    errorMsg,
                    null,
                    null
                );
            }
            
            // Check if executable is actually executable
            if (!Files.isExecutable(executablePath)) {
                logger.warn("Executable may not have execute permissions: {}", executablePath);
            }
            
            // Create output directory
            Files.createDirectories(outputDir);
            logger.info("Created output directory: {}", outputDir);
            
            // Write initial conditions to input file
            writeInputFile(inputFile, initialConditions);
            logger.info("Created input file: {}", inputFile);
            
            // Execute Fortran program
            // The Fortran program expects: ./m.exe <input_file>
            ProcessBuilder processBuilder = new ProcessBuilder(
                executablePath.toString(),
                inputFile.toString()
            );
            
            processBuilder.directory(workDir.toFile());
            processBuilder.redirectErrorStream(true);
            
            logger.info("Executing: {} {}", executablePath, inputFile);
            Process process = processBuilder.start();
            
            // Read output
            StringBuilder output = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    output.append(line).append("\n");
                }
            }
            
            // Wait for process with timeout
            boolean finished = process.waitFor(MAX_EXECUTION_TIME, TimeUnit.SECONDS);
            
            if (!finished) {
                process.destroyForcibly();
                return new FortranExecutionResult(
                    executionId,
                    false,
                    "Execution timeout after " + MAX_EXECUTION_TIME + " seconds",
                    output.toString(),
                    null
                );
            }
            
            int exitCode = process.exitValue();
            
            // Collect output files
            List<String> outputFiles = collectOutputFiles(outputDir);
            
            if (exitCode == 0) {
                String successMessage = String.format(
                    "Calculation completed successfully! Generated %d output file(s). Execution ID: %s",
                    outputFiles.size(),
                    executionId.substring(0, 8) + "..."
                );
                return new FortranExecutionResult(
                    executionId,
                    true,
                    successMessage,
                    output.toString(),
                    outputFiles
                );
            } else {
                return new FortranExecutionResult(
                    executionId,
                    false,
                    "Fortran program exited with error code " + exitCode + ". Check output for details.",
                    output.toString(),
                    outputFiles
                );
            }
            
        } catch (IOException e) {
            logger.error("IO Error executing Fortran: ", e);
            return new FortranExecutionResult(
                executionId,
                false,
                "IO Error: " + e.getMessage() + ". Check if executable exists and has permissions.",
                e.getClass().getSimpleName() + ": " + e.getMessage(),
                null
            );
        } catch (InterruptedException e) {
            logger.error("Interrupted while executing Fortran: ", e);
            Thread.currentThread().interrupt();
            return new FortranExecutionResult(
                executionId,
                false,
                "Execution interrupted: " + e.getMessage(),
                null,
                null
            );
        } catch (Exception e) {
            logger.error("Unexpected error executing Fortran: ", e);
            String errorDetails = e.getClass().getSimpleName() + ": " + e.getMessage();
            if (e.getCause() != null) {
                errorDetails += " (Caused by: " + e.getCause().getMessage() + ")";
            }
            return new FortranExecutionResult(
                executionId,
                false,
                "Error: " + e.getMessage(),
                errorDetails,
                null
            );
        }
    }
    
    /**
     * Write initial conditions to input file for Fortran program
     */
    private void writeInputFile(Path inputFile, InitialConditionsDTO conditions) throws IOException {
        try (BufferedWriter writer = Files.newBufferedWriter(inputFile)) {
            // Write number of fields
            int numFields = conditions.getFieldValues().size();
            writer.write(String.valueOf(numFields));
            writer.newLine();
            
            // Write field values
            for (Double value : conditions.getFieldValues()) {
                writer.write(String.valueOf(value));
                writer.newLine();
            }
            
            // Write field velocities
            for (Double velocity : conditions.getFieldVelocities()) {
                writer.write(String.valueOf(velocity));
                writer.newLine();
            }
            
            // Write parameters
            writer.write(String.valueOf(conditions.getInitialTime()));
            writer.newLine();
            writer.write(String.valueOf(conditions.getTimeStep()));
            writer.newLine();
            writer.write(String.valueOf(conditions.getKstar()));
            writer.newLine();
            writer.write(String.valueOf(conditions.getCq()));
            writer.newLine();
            
            // Write potential type
            writer.write(conditions.getPotentialType() != null ? conditions.getPotentialType() : "tanh");
            writer.newLine();
            
            // Write potential parameters
            if (conditions.getPotentialParameters() != null && !conditions.getPotentialParameters().isEmpty()) {
                writer.write(String.valueOf(conditions.getPotentialParameters().size()));
                writer.newLine();
                for (Double param : conditions.getPotentialParameters()) {
                    writer.write(String.valueOf(param));
                    writer.newLine();
                }
            } else {
                writer.write("0"); // No parameters
                writer.newLine();
            }
            
            // Write potential expression (if custom)
            if (conditions.getPotentialExpression() != null) {
                writer.write(conditions.getPotentialExpression());
                writer.newLine();
            } else {
                writer.write(""); // Empty string
                writer.newLine();
            }
        }
    }
    
    /**
     * Collect output files from execution directory
     */
    private List<String> collectOutputFiles(Path outputDir) throws IOException {
        if (!Files.exists(outputDir)) {
            logger.warn("Output directory does not exist: {}", outputDir);
            return new ArrayList<>();
        }
        
        List<String> files = new ArrayList<>();
        try {
            Files.walk(outputDir)
                .filter(Files::isRegularFile)
                .forEach(path -> files.add(outputDir.relativize(path).toString()));
        } catch (IOException e) {
            logger.error("Error collecting output files: ", e);
            throw e;
        }
        return files;
    }
    
    /**
     * Result of Fortran execution
     */
    public static class FortranExecutionResult {
        private String executionId;
        private boolean success;
        private String message;
        private String output;
        private List<String> outputFiles;
        
        public FortranExecutionResult(String executionId, boolean success, 
                                     String message, String output, 
                                     List<String> outputFiles) {
            this.executionId = executionId;
            this.success = success;
            this.message = message;
            this.output = output;
            this.outputFiles = outputFiles;
        }
        
        // Getters
        public String getExecutionId() { return executionId; }
        public boolean isSuccess() { return success; }
        public String getMessage() { return message; }
        public String getOutput() { return output; }
        public List<String> getOutputFiles() { return outputFiles; }
    }
}

