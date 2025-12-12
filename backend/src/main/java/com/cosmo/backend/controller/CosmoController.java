package com.cosmo.backend.controller;

import com.cosmo.backend.dto.InitialConditionsDTO;
import com.cosmo.backend.service.FortranExecutionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

//Logging API (prints to conslole/logs in a structures way)
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

//Rest Controller for Cosmological Perturbation Calculations

@RestController // this class contains HTTP endpoints
@RequestMapping("/api/cosmo-perturbations")


public class CosmoController {

    private static final Logger logger = LoggerFactory.getLogger(CosmoController.class);  // Changed here
    //creates a logger named after this class | static =one logger shared with all instances |
    //                                         final =cannot be reassigned

    @Autowired //
    private FortranExecutionService fortranExecutionService;
    //Spring will autoamtically inject an instance of  FortranExecutionService
    // --> no new private FortranExecutionService will created

    //Execute cosmological perturbations calculation
    //POST /api/cosmo-perturbations/calculate

    /**
     * @param initialConditions Initial conditions from frontend
     * @return Execution result
     */

    @PostMapping("/calculate")
    public ResponseEntity<FortranExecutionService.FortranExecutionResult> calculate(
            @RequestBody InitialConditionsDTO initialConditions) {

        //ResponseEntity <..> --> return both a body and a HTTP status code
        //@RequestBody ---> take the JSON body from the frontend and convert it to InitialConditionsDTO

        logger.info("Received calculation request"); // print an information log message
        
        //try/catch = protect the endpoint so the backend doesnt crash if something goes wrong
        try {
            // Validate input --> check if the input is valid
            if (initialConditions.getFieldValues() == null || 
                initialConditions.getFieldValues().isEmpty()) {
                logger.warn("Validation failed: fieldValues is null or empty");
                return ResponseEntity.badRequest().build();
            }
            
            if (initialConditions.getFieldVelocities() == null || 
                initialConditions.getFieldVelocities().isEmpty()) {
                logger.warn("Validation failed: fieldVelocities is null or empty");
                return ResponseEntity.badRequest().build();
            }
            
            if (initialConditions.getFieldValues().size() != 
                initialConditions.getFieldVelocities().size()) {
                logger.warn("Validation failed: fieldValues size ({}) != fieldVelocities size ({})",
                    initialConditions.getFieldValues().size(),
                    initialConditions.getFieldVelocities().size());
                return ResponseEntity.badRequest().build();
            }
            
            // We passed validation, so now we try running Fortran.
            logger.info("Input validated. 🚀 Executing Fortran program...");
            
            // Execute Fortran program
            FortranExecutionService.FortranExecutionResult result = 
                fortranExecutionService.executeFortran(initialConditions);
            
            logger.info("🏺Fortran execution completed. Success: {}, Message: {}", 
                result.isSuccess(), result.getMessage());
            
            if (result.isSuccess()) {
                return ResponseEntity.ok(result);
            } else {
                // Return 500 with error details
                logger.error("Fortran execution failed: {}", result.getMessage());
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(result);
            }
        } catch (Exception e) {
            logger.error("Unexpected error in calculate endpoint: ", e);
            FortranExecutionService.FortranExecutionResult errorResult = 
                new FortranExecutionService.FortranExecutionResult(
                    "error",
                    false,
                    "Unexpected error: " + e.getMessage(),
                    e.getClass().getSimpleName() + ": " + e.getMessage(),
                    null
                );
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResult);
        }
    }
    
    // Health check endpoint
    // GET /api/cosmo-perturbations/health --> this is a simple endpoint to check if the backend is runnin
     
    @GetMapping("/health")
    public ResponseEntity<String> health() {
        return ResponseEntity.ok("Cosmological Perturbations API is running");  // Updated message
    }
    
    /**
     * Test endpoint to check Fortran executable
     * GET /api/cosmo-perturbations/check-executable
     */
    @GetMapping("/check-executable")
    public ResponseEntity<String> checkExecutable() {
        try {
            java.nio.file.Path workDir = java.nio.file.Paths.get("./spare").toAbsolutePath();
            java.nio.file.Path executable = workDir.resolve("m.exe");
            
            StringBuilder response = new StringBuilder();
            response.append("Checking Fortran executable...\n");
            response.append("Work directory: ").append(workDir).append("\n");
            response.append("Executable path: ").append(executable).append("\n");
            response.append("Exists: ").append(java.nio.file.Files.exists(executable)).append("\n");
            
            if (java.nio.file.Files.exists(executable)) {
                response.append("Is executable: ").append(java.nio.file.Files.isExecutable(executable)).append("\n");
                response.append("Size: ").append(java.nio.file.Files.size(executable)).append(" bytes\n");
            } else {
                response.append("ERROR: Executable not found!\n");
                response.append("Please compile: cd spare && gfortran gravitationalwaves.f -o m.exe\n");
            }
            
            return ResponseEntity.ok(response.toString());
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body("Error checking executable: " + e.getMessage());
        }
    }
}
