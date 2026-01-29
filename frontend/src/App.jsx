// src/App.jsx
// We import React's useState so we can store changing values.
import { useState, useEffect, useRef } from "react";
import "./App.css";

// API base URL - use environment variable in production, or relative path in development
const API_BASE_URL = import.meta.env.VITE_API_URL || '';

function App() {
  // Which page is currently visible: "model", "initial", "summary", "about", "instructions", "notesList", or "pdfViewer"
  const [activePage, setActivePage] = useState("about");
  // Track which PDF is being viewed
  const [viewingPdf, setViewingPdf] = useState(null);
  // Number of scalar fields
  const [numFields, setNumFields] = useState(1);
  // Display value for number of fields (allows typing)
  const [numFieldsInput, setNumFieldsInput] = useState("1");
  // Potential V(phi) - Starobinsky potential
  const [potential, setPotential] = useState(" (1 - exp(-sqrt(2/3) * x(1)))**2");
  // Arrays for initial values and velocities of each field
  const [initialValues, setInitialValues] = useState([""]);
  const [initialVelocities, setInitialVelocities] = useState([""]);
  // Initial time (for example N = 0 or t = 0)
  const [initialTime, setInitialTime] = useState("0.0");
  // Number of parameters
  const [numParameters, setNumParameters] = useState(0);
  // Display value for number of parameters (allows typing)
  const [numParametersInput, setNumParametersInput] = useState("0");
  // Array of parameter objects: [{name: "m", value: "1.0"}, ...]
  const [parameters, setParameters] = useState([]);
  // Metric matrix: 2D array (strings) for n×n metric expressions.
  // Examples: "1", "0", "x(1)**2"
  const [metric, setMetric] = useState([["1.0"]]);

  // ----------------------------------------------------
  //  update number of fields AND keep arrays in sync
  // ----------------------------------------------------
  function handleNumFieldsChange(event) {
    // event.target.value is a string from the input
    const valueAsString = event.target.value;

    // Update the input display value to allow free typing
    setNumFieldsInput(valueAsString);

    // Convert to integer
    let n = parseInt(valueAsString, 10);

    // If conversion fails or n < 1, don't update the actual number or arrays
    if (isNaN(n) || n < 1) {
      return;
    }

    // Update state for number of fields
    setNumFields(n);

    // Update initialValues array to have length n
    setInitialValues((previousArray) => {
      const newArray = [...previousArray]; // copy old values

      if (n > newArray.length) {
        // If we need more fields, add empty strings
        while (newArray.length < n) {
          newArray.push("");
        }
      } else if (n < newArray.length) {
        // If fewer fields, cut the extra ones
        newArray.length = n;
      }

      return newArray;
    });

    // Update initialVelocities array to have length n
    setInitialVelocities((previousArray) => {
      const newArray = [...previousArray];

      if (n > newArray.length) {
        while (newArray.length < n) {
          newArray.push("");
        }
      } else if (n < newArray.length) {
        newArray.length = n;
      }

      return newArray;
    });

    // Update metric matrix to be n×n
    setMetric((previousMetric) => {
      const newMetric = [];
      for (let i = 0; i < n; i++) {
        const row = [];
        for (let j = 0; j < n; j++) {
          // If previous metric exists and indices are valid, use old value, otherwise default
          if (previousMetric[i] && previousMetric[i][j] !== undefined) {
            row.push(previousMetric[i][j]);
          } else {
            // Default: identity matrix (1 on diagonal, 0 off-diagonal)
            row.push(i === j ? "1.0" : "0.0");
          }
        }
        newMetric.push(row);
      }
      return newMetric;
    });
  }

  // ----------------------------------------------------
  // Update value of field i
  // ----------------------------------------------------
  function handleFieldValueChange(index, event) {
    const newValue = event.target.value;
    const copy = [...initialValues]; // copy the array
    copy[index] = newValue;          // change only one position
    setInitialValues(copy);          // save new array
  }

  // ----------------------------------------------------
  // Update velocity of field i
  // ----------------------------------------------------
  function handleFieldVelocityChange(index, event) {
    const newValue = event.target.value;
    const copy = [...initialVelocities];
    copy[index] = newValue;
    setInitialVelocities(copy);
  }

  // ----------------------------------------------------
  // Update number of parameters AND keep array in sync
  // ----------------------------------------------------
  function handleNumParametersChange(event) {
    const valueAsString = event.target.value;
    
    // Update the input display value to allow free typing
    setNumParametersInput(valueAsString);
    
    let n = parseInt(valueAsString, 10);
    
    // If conversion fails or n < 0, don't update the actual number or arrays
    if (isNaN(n) || n < 0) {
      return;
    }
    
    setNumParameters(n);
    
    // Update parameters array to have length n
    setParameters((previousArray) => {
      const newArray = [...previousArray];
      
      if (n > newArray.length) {
        // If we need more parameters, add empty ones with empty value (user can enter their own)
        while (newArray.length < n) {
          newArray.push({ name: "", value: "" });
        }
      } else if (n < newArray.length) {
        // If fewer parameters, cut the extra ones
        newArray.length = n;
      }
      
      return newArray;
    });
  }

  // ----------------------------------------------------
  // Update parameter name at index i
  // ----------------------------------------------------
  function handleParameterNameChange(index, event) {
    const newName = event.target.value;
    const copy = [...parameters];
    copy[index] = { ...copy[index], name: newName };
    setParameters(copy);
  }

  // ----------------------------------------------------
  // Update parameter value at index i
  // ----------------------------------------------------
  function handleParameterValueChange(index, event) {
    const newValue = event.target.value;
    const copy = [...parameters];
    // Allow empty values - don't force "0"
    copy[index] = { ...copy[index], value: newValue };
    setParameters(copy);
  }

  // ----------------------------------------------------
  // Update metric value at position (i, j)
  // ----------------------------------------------------
  function handleMetricChange(i, j, event) {
    const newValue = event.target.value;
    const copy = metric.map(row => [...row]);
    // Keep metric entries as raw expressions (strings).
    // Backend/Fortran will interpret these expressions.
    copy[i][j] = newValue;
    setMetric(copy);
  }

  // ----------------------------------------------------
  // State for calculation results
  // ----------------------------------------------------
  const [calculationResult, setCalculationResult] = useState(null);
  const [isCalculating, setIsCalculating] = useState(false);
  const [error, setError] = useState(null);
  const [currentExecutionId, setCurrentExecutionId] = useState(null);
  const [abortController, setAbortController] = useState(null);
  const isCancelledRef = useRef(false);
  const pendingExecutionIdRef = useRef(null);

  // ----------------------------------------------------
  // Prevent logo from being copied to clipboard
  // ----------------------------------------------------
  useEffect(() => {
    const handleCopy = (e) => {
      // Get the selected text
      const selection = window.getSelection().toString();
      
      // If there's selected text, set it to clipboard without any logo
      if (selection) {
        e.clipboardData.setData('text/plain', selection);
        e.preventDefault();
      }
    };

    document.addEventListener('copy', handleCopy);
    
    return () => {
      document.removeEventListener('copy', handleCopy);
    };
  }, []);

  // ----------------------------------------------------
  // When user clicks the "Run Calculation" button
  // ----------------------------------------------------
  async function handleRunCalculation() {
    // Validate inputs
    const fieldValues = initialValues
      .map(v => parseFloat(v))
      .filter(v => !isNaN(v));
    
    const fieldVelocities = initialVelocities
      .map(v => parseFloat(v))
      .filter(v => !isNaN(v));
    
    if (fieldValues.length === 0 || fieldVelocities.length === 0) {
      alert("Please enter valid field values and velocities!");
      return;
    }
    
    if (fieldValues.length !== fieldVelocities.length) {
      alert("Number of field values must match number of velocities!");
      return;
    }
    
    setIsCalculating(true);
    setError(null);
    isCancelledRef.current = false;
    // Clear previous result immediately to prevent showing old plot
    setCalculationResult(null);
    setCurrentExecutionId(null);
    pendingExecutionIdRef.current = null;
    
    // Force a small delay to ensure React re-renders and clears the old image
    // This helps prevent showing cached images
    await new Promise(resolve => setTimeout(resolve, 100));
    
    // Create AbortController for cancellation
    const controller = new AbortController();
    setAbortController(controller);
    
    // Generate a temporary execution ID for tracking (in case we need to cancel before response)
    const tempExecutionId = `pending_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
    pendingExecutionIdRef.current = tempExecutionId;
    
    // Prepare parameters object from the parameters array
    // Only include parameters with valid names and numeric values
    const parametersObj = {};
    parameters.forEach(param => {
      if (param.name && param.name.trim() !== "") {
        const valueStr = param.value ? param.value.trim() : "";
        if (valueStr !== "") {
          const value = parseFloat(valueStr);
          if (!isNaN(value)) {
            parametersObj[param.name] = value;
          }
        }
      }
    });

    // Prepare request data
    const requestData = {
      fieldValues: fieldValues,
      fieldVelocities: fieldVelocities,
      initialTime: parseFloat(initialTime) || 0.0,
      timeStep: 0.05,
      kstar: 0.05,
      cq: 100.0,
      potentialType: "tanh",
      potentialParameters: [
        0.1,                    // a0 (base coefficient)
        0.0007,                 // a1
        0.0007,                 // a2
        0.0007,                 // a3
        0.0007,                 // a4
        -0.9811333867328498,    // b1
        -0.9787014277530225,    // b2
        -0.9759597865125814,    // b3
        -0.972870068569409      // b4
      ],
      potentialExpression: potential || "",
      parameterValues: parametersObj,
      numParameters: numParameters,
      metric: metric
    };
    
    try {
      const response = await fetch(`${API_BASE_URL}/api/cosmo-perturbations/calculate`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(requestData),
        signal: controller.signal
      });
      
      // Check if cancelled before processing
      if (isCancelledRef.current || controller.signal.aborted) {
        return;
      }
      
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      
      // Get execution ID from response (new async format)
      const initialResponse = await response.json();
      const executionId = initialResponse.executionId || response.headers.get('X-Execution-Id');
      
      if (!executionId) {
        throw new Error("No execution ID received from server");
      }
      
      // Set execution ID immediately for cancellation support
      setCurrentExecutionId(executionId);
      pendingExecutionIdRef.current = executionId;
      console.log("Got execution ID:", executionId);
      
      // Check if cancelled after getting execution ID
      if (isCancelledRef.current || controller.signal.aborted) {
        return;
      }
      
      // Poll for status until completion or cancellation
      const pollInterval = 1000; // Poll every 1 second
      const maxPollTime = 30 * 60 * 1000; // Max 30 minutes
      const startTime = Date.now();
      
      while (true) {
        // Check if cancelled
        if (isCancelledRef.current || controller.signal.aborted) {
          console.log("Polling cancelled");
          return;
        }
        
        // Check timeout
        if (Date.now() - startTime > maxPollTime) {
          throw new Error("Calculation timeout - exceeded maximum polling time");
        }
        
        // Poll for status
        try {
          const statusResponse = await fetch(`${API_BASE_URL}/api/cosmo-perturbations/status/${executionId}`, {
            signal: controller.signal
          });
          
          if (!statusResponse.ok) {
            throw new Error(`Status check failed: ${statusResponse.status}`);
          }
          
          const statusResult = await statusResponse.json();
          
          // Check if cancelled during fetch
          if (isCancelledRef.current || controller.signal.aborted) {
            return;
          }
          
          if (statusResult.status === "completed" || statusResult.status === "failed") {
            // Calculation finished
            const result = {
              executionId: executionId,
              success: statusResult.success,
              message: statusResult.message,
              output: statusResult.output,
              outputFiles: statusResult.outputFiles
            };
            
            setCalculationResult(result);
            
            if (result.success) {
              const fileCount = result.outputFiles ? result.outputFiles.length : 0;
              alert(`✅ Calculation completed successfully!\n\nGenerated ${fileCount} output file(s).\n\nCheck the results below for details.`);
            } else {
              alert("❌ Calculation failed: " + result.message + "\n\nCheck the error details below.");
            }
            
            break; // Exit polling loop
          } else if (statusResult.status === "cancelled") {
            // Already cancelled
            console.log("Calculation was cancelled");
            return;
          } else {
            // Still running, wait and poll again
            await new Promise(resolve => setTimeout(resolve, pollInterval));
          }
        } catch (pollErr) {
          if (pollErr.name === 'AbortError' || isCancelledRef.current) {
            console.log("Polling aborted");
            return;
          }
          // Continue polling on other errors
          console.warn("Polling error:", pollErr);
          await new Promise(resolve => setTimeout(resolve, pollInterval));
        }
      }
    } catch (err) {
      // If aborted or cancelled, don't update state (cancel function already handled it)
      if (err.name === 'AbortError' || isCancelledRef.current) {
        console.log("Calculation was cancelled");
        return;
      }
      
      // Only handle other errors if not cancelled
      if (!isCancelledRef.current && !controller.signal.aborted) {
        setError(err.message);
        alert("Error: " + err.message);
        console.error("Error:", err);
      }
    } finally {
      // Only update state if not cancelled (cancel function handles cancelled state)
      if (!isCancelledRef.current && !controller.signal.aborted) {
        setIsCalculating(false);
        setAbortController(null);
      }
    }
  }

  // ----------------------------------------------------
  // Cancel running calculation
  // ----------------------------------------------------
  async function handleCancelCalculation() {
    // Set cancelled flag immediately to prevent any further processing
    isCancelledRef.current = true;
    
    // Immediately update UI to prevent further interactions
    setIsCalculating(false);
    // Don't set error message - we don't want to show anything when cancelled
    setError(null);
    
    // Store the current execution ID and abort controller before clearing
    // Try to get execution ID from state, pending ref, or use a fallback
    const execId = currentExecutionId || pendingExecutionIdRef.current;
    const controller = abortController;
    
    console.log("Cancelling calculation. Execution ID:", execId);
    
    // Abort the fetch request FIRST (this will interrupt the HTTP request)
    if (controller) {
      controller.abort();
      console.log("Aborted fetch request");
    }
    
    // Call backend cancel endpoint to kill the process
    // Use the execution ID we have
    const execIdToCancel = execId || pendingExecutionIdRef.current;
    let cancelSuccess = false;
    
    if (execIdToCancel && !execIdToCancel.startsWith('pending_')) {
      try {
        console.log(`Attempting to cancel execution: ${execIdToCancel}`);
        const response = await fetch(`${API_BASE_URL}/api/cosmo-perturbations/cancel/${execIdToCancel}`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          }
        });
        
        if (response.ok) {
          const result = await response.json();
          if (result.success) {
            console.log(`✅ Backend process ${execIdToCancel} cancelled successfully`);
            cancelSuccess = true;
          } else {
            console.warn(`Backend cancel returned:`, result.message);
          }
        } else {
          const errorResult = await response.json().catch(() => ({}));
          console.warn(`Backend cancel endpoint returned status: ${response.status}`, errorResult.message || '');
        }
      } catch (err) {
        console.error(`Error calling cancel endpoint:`, err);
      }
    } else {
      console.warn("⚠️ No valid execution ID available for cancellation.");
    }
    
    // Clear state immediately
    setAbortController(null);
    setCurrentExecutionId(null);
    pendingExecutionIdRef.current = null;
    
    // Don't set calculation result for cancelled calculations - just clear it
    // This way the result panel won't be shown
    setCalculationResult(null);
    setError(null);
    
    if (cancelSuccess) {
      alert("✅ Calculation cancelled successfully!");
    } else {
      alert("⚠️ Calculation cancellation requested, but backend process may still be running.");
    }
  }

  // ----------------------------------------------------
  // When user clicks the "Preview configuration" button
  // ----------------------------------------------------
  //function handlePreviewConfig() {
   // const config = {
   //   numFields: numFields,
   //   potential: potential,
   //   numParameters: numParameters,
   //   parameters: parameters,
   //   initialTime: initialTime,
   //   initialValues: initialValues,
   //   initialVelocities: initialVelocities,
  //  };

    // Show the configuration in the browser console
  // console.log("Cosmological perturbation config:", config);

    // Show a simple message on the screen
    //alert("Parameters collected! Click 'Run Calculation' to execute.");
 // }

  // ----------------------------------------------------
  // JSX: what appears on the screen
  // ----------------------------------------------------
  return (
    <div className="app">
      {/* ================== LEFT SIDEBAR ================== */}
      <aside className="sidebar">
        <div style={{ textAlign: "center", marginBottom: "20px" }}>
          <img 
            src="/unnamed1.jpg" 
            alt="Eagle Logo" 
            style={{
              width: "100px",
              height: "100px",
              objectFit: "contain",
              userSelect: "none",
              WebkitUserSelect: "none",
              MozUserSelect: "none",
              msUserSelect: "none",
              pointerEvents: "none"
            }}
            draggable="false"
          />
        </div>
        <div className="menu-title">Menu</div>

        <nav className="menu-list">
          {/* Button: Notes for fluctuations */}
          <button
            className={`menu-item ${activePage === "notesList" || activePage === "pdfViewer" ? "active" : ""}`}
            onClick={() => setActivePage("notesList")}
          >
            Notes for fluctuations
          </button>

          {/* Button: How to use */}
          <button
            className={`menu-item ${activePage === "instructions" ? "active" : ""}`}
            onClick={() => setActivePage("instructions")}
          >
            How to use
          </button>
        </nav>
      </aside>

      {/* ================== MAIN CONTENT ================== */}
      <main className="content">
        {/* Top banner */}
        <section className="header-banner">
          <h1 className="name-title">Cosmological Perturbations</h1>
          <p className="header-subtitle">
            Define your multi-field inflation model and initial conditions. This app 
            will give you the infaltionary dynamics and the power spectrum.
          </p>
        </section>

        {/* Horizontal Navigation Menu */}
        <nav className="horizontal-nav">
          <button
            className={`nav-button ${activePage === "about" ? "active" : ""}`}
            onClick={() => setActivePage("about")}
          >
            About
          </button>
          <button
            className={`nav-button ${activePage === "model" ? "active" : ""}`}
            onClick={() => setActivePage("model")}
          >
            Model setup
          </button>
          <button
            className={`nav-button ${activePage === "initial" ? "active" : ""}`}
            onClick={() => setActivePage("initial")}
          >
            Initial conditions
          </button>
          <button
            className={`nav-button ${activePage === "summary" ? "active" : ""}`}
            onClick={() => setActivePage("summary")}
          >
            Summary / Export
          </button>
        </nav>

        {/* ================== PAGE: ABOUT ================== */}
        {activePage === "about" && (
          <section className="section">
            <h2 className="section-title">About this app</h2>
            <p className="section-text">
              This app provides the cosmological perturbations 
              for a given inflationary potential and initial condition. The idea is:
            </p>
            <ol className="section-list">
              <li>Define the inflationary potential, the metric matrix in field space and model parameters.</li>
              <li>Specify initial conditions for the background fields.</li>
              <li>
                Send the configuration to a Fortran backend that integrates the
                background and perturbation equations.
              </li>
              <li>
                Visualize power spectra and other observables directly in the
                browser.
              </li>
            </ol>

            {/* Navigation button */}
            <div style={{ display: 'flex', justifyContent: 'flex-end', marginTop: '30px' }}>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("model")}
                style={{ 
                  backgroundColor: '#00ffc3',
                  color: '#0a0a0a',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: 'none',
                  cursor: 'pointer',
                  borderRadius: '8px',
                  transition: 'all 0.2s ease'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = '#00e5b3';
                  e.target.style.transform = 'translateY(-1px)';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = '#00ffc3';
                  e.target.style.transform = 'translateY(0)';
                }}
              >
                Next →
              </button>
            </div>
          </section>
        )}

        {/* ================== PAGE: NOTES LIST ================== */}
        {activePage === "notesList" && (
          <section className="section">
            <h2 className="section-title">Available Notes</h2>
            
            <div style={{ 
              marginTop: '30px', 
              padding: '30px', 
              backgroundColor: '#0a0a0a', 
              borderRadius: '8px', 
              border: '2px solid rgba(0, 255, 195, 0.5)',
              boxShadow: '0 0 20px rgba(0, 255, 195, 0.3)',
              maxWidth: '95%' 
            }}>
              <div 
                onClick={() => {
                  setViewingPdf("/fluctuatiions.pdf");
                  setActivePage("pdfViewer");
                }}
                style={{
                  display: 'flex',
                  alignItems: 'center',
                  gap: '15px',
                  padding: '16px 20px',
                  backgroundColor: '#050505',
                  borderRadius: '8px',
                  border: '1px solid rgba(0, 191, 166, 0.3)',
                  cursor: 'pointer',
                  transition: 'all 0.2s ease',
                  marginBottom: '12px'
                }}
                onMouseEnter={(e) => {
                  e.currentTarget.style.backgroundColor = '#0a0a0a';
                  e.currentTarget.style.borderColor = '#00ffc3';
                  e.currentTarget.style.boxShadow = '0 0 10px rgba(0, 255, 195, 0.2)';
                }}
                onMouseLeave={(e) => {
                  e.currentTarget.style.backgroundColor = '#050505';
                  e.currentTarget.style.borderColor = 'rgba(0, 191, 166, 0.3)';
                  e.currentTarget.style.boxShadow = 'none';
                }}
              >
                <svg 
                  width="24" 
                  height="24" 
                  viewBox="0 0 24 24" 
                  fill="none" 
                  stroke="#00ffc3" 
                  strokeWidth="2"
                  style={{ flexShrink: 0 }}
                >
                  <path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z" />
                  <polyline points="14 2 14 8 20 8" />
                  <line x1="16" y1="13" x2="8" y2="13" />
                  <line x1="16" y1="17" x2="8" y2="17" />
                  <polyline points="10 9 9 9 8 9" />
                </svg>
                <span style={{ 
                  color: '#e8fff7', 
                  fontSize: '16px',
                  fontWeight: '500'
                }}>
                  Notes of cosmological fluctuations
                </span>
              </div>
            </div>
          </section>
        )}

        {/* ================== PAGE: PDF VIEWER ================== */}
        {activePage === "pdfViewer" && viewingPdf && (
          <section className="section" style={{ padding: '0' }}>
            <div style={{ 
              padding: '20px',
              borderBottom: '1px solid rgba(0, 191, 166, 0.3)',
              backgroundColor: '#0a0a0a'
            }}>
              <button
                onClick={() => setActivePage("notesList")}
                style={{
                  backgroundColor: 'transparent',
                  border: '1px solid rgba(0, 191, 166, 0.5)',
                  color: '#00ffc3',
                  padding: '8px 16px',
                  borderRadius: '6px',
                  cursor: 'pointer',
                  fontSize: '14px',
                  marginBottom: '15px',
                  transition: 'all 0.2s ease'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = 'rgba(0, 191, 166, 0.1)';
                  e.target.style.borderColor = '#00ffc3';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = 'transparent';
                  e.target.style.borderColor = 'rgba(0, 191, 166, 0.5)';
                }}
              >
                ← Back to Notes List
              </button>
              
              <h2 style={{ 
                color: '#00ffc3', 
                margin: '10px 0',
                fontSize: '24px'
              }}>
                Notes of cosmological fluctuations
              </h2>
              
              <div style={{
                display: 'flex',
                alignItems: 'center',
                gap: '15px',
                marginTop: '15px',
                flexWrap: 'wrap'
              }}>
                <span style={{ 
                  color: '#91fff3', 
                  fontSize: '14px'
                }}>
                  {viewingPdf.split('/').pop()}
                </span>
                
                <div style={{
                  display: 'flex',
                  gap: '10px',
                  alignItems: 'center'
                }}>
                  <button
                    onClick={() => window.open(viewingPdf, '_blank')}
                    style={{
                      backgroundColor: 'rgba(0, 191, 166, 0.2)',
                      border: '1px solid rgba(0, 191, 166, 0.5)',
                      color: '#00ffc3',
                      padding: '6px 12px',
                      borderRadius: '4px',
                      cursor: 'pointer',
                      fontSize: '13px',
                      display: 'flex',
                      alignItems: 'center',
                      gap: '6px',
                      transition: 'all 0.2s ease'
                    }}
                    onMouseEnter={(e) => {
                      e.target.style.backgroundColor = 'rgba(0, 191, 166, 0.3)';
                    }}
                    onMouseLeave={(e) => {
                      e.target.style.backgroundColor = 'rgba(0, 191, 166, 0.2)';
                    }}
                    title="Open in new window"
                  >
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                      <path d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6" />
                      <polyline points="15 3 21 3 21 9" />
                      <line x1="10" y1="14" x2="21" y2="3" />
                    </svg>
                    Open in new window
                  </button>
                  
                  <a
                    href={viewingPdf}
                    download
                    style={{
                      backgroundColor: 'rgba(0, 191, 166, 0.2)',
                      border: '1px solid rgba(0, 191, 166, 0.5)',
                      color: '#00ffc3',
                      padding: '6px 12px',
                      borderRadius: '4px',
                      cursor: 'pointer',
                      fontSize: '13px',
                      display: 'flex',
                      alignItems: 'center',
                      gap: '6px',
                      textDecoration: 'none',
                      transition: 'all 0.2s ease'
                    }}
                    onMouseEnter={(e) => {
                      e.target.style.backgroundColor = 'rgba(0, 191, 166, 0.3)';
                    }}
                    onMouseLeave={(e) => {
                      e.target.style.backgroundColor = 'rgba(0, 191, 166, 0.2)';
                    }}
                    title="Download PDF"
                  >
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                      <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" />
                      <polyline points="7 10 12 15 17 10" />
                      <line x1="12" y1="15" x2="12" y2="3" />
                    </svg>
                    Download
                  </a>
                </div>
              </div>
            </div>
            
            <div style={{
              width: '100%',
              height: 'calc(100vh - 300px)',
              minHeight: '600px',
              backgroundColor: '#1a1a1a',
              border: '1px solid rgba(0, 191, 166, 0.3)'
            }}>
              <iframe
                src={`${viewingPdf}#toolbar=1&navpanes=1&scrollbar=1`}
                style={{
                  width: '100%',
                  height: '100%',
                  border: 'none'
                }}
                title="PDF Viewer"
              />
            </div>
          </section>
        )}

        {/* ================== PAGE: INSTRUCTIONS ================== */}
        {activePage === "instructions" && (
          <section className="section">
            <h2 className="section-title">Instructions</h2>
            
            <div style={{ 
              marginTop: '20px', 
              padding: '20px', 
              backgroundColor: '#0a0a0a', 
              borderRadius: '4px', 
              border: '1px solid rgba(0, 191, 166, 0.3)', 
              maxWidth: '95%' 
            }}>
              <h3 style={{ 
                color: '#00ffc3', 
                fontSize: '20px', 
                marginBottom: '16px',
                marginTop: '0'
              }}>
                Writing Potential Expressions
              </h3>
              
              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Field Notation
                </h4>
                <ul style={{ 
                  color: '#e8fff7', 
                  fontSize: '14px',
                  lineHeight: '1.8',
                  paddingLeft: '20px',
                  margin: '0'
                }}>
                  <li><code style={{ color: '#91fff3' }}>x(1)</code> represents field 1</li>
                  <li><code style={{ color: '#91fff3' }}>x(2)</code> represents field 2</li>
                  <li><code style={{ color: '#91fff3' }}>x(3)</code> represents field 3</li>
                  <li>And so on for additional fields...</li>
                </ul>
              </div>

              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Mathematical Operators
                </h4>
                <ul style={{ 
                  color: '#e8fff7', 
                  fontSize: '14px',
                  lineHeight: '1.8',
                  paddingLeft: '20px',
                  margin: '0'
                }}>
                  <li>Use <code style={{ color: '#91fff3' }}>**</code> for exponentiation, not <code style={{ color: '#91fff3' }}>^</code></li>
                  <li>Example: <code style={{ color: '#91fff3' }}>10**2</code> = 100 (not <code style={{ color: '#91fff3' }}>10^2</code>)</li>
                  <li>Use <code style={{ color: '#91fff3' }}>*</code> for multiplication</li>
                  <li>Use <code style={{ color: '#91fff3' }}>/</code> for division</li>
                  <li>Use <code style={{ color: '#91fff3' }}>+</code> and <code style={{ color: '#91fff3' }}>-</code> for addition and subtraction</li>
                </ul>
              </div>

              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Common Functions
                </h4>
                <ul style={{ 
                  color: '#e8fff7', 
                  fontSize: '14px',
                  lineHeight: '1.8',
                  paddingLeft: '20px',
                  margin: '0'
                }}>
                  <li><code style={{ color: '#91fff3' }}>Sin(x)</code>, <code style={{ color: '#91fff3' }}>Cos(x)</code>, <code style={{ color: '#91fff3' }}>Tan(x)</code></li>
                  <li><code style={{ color: '#91fff3' }}>Sinh(x)</code>, <code style={{ color: '#91fff3' }}>Cosh(x)</code>, <code style={{ color: '#91fff3' }}>Tanh(x)</code></li>
                  <li><code style={{ color: '#91fff3' }}>Sqrt(x)</code> for square root</li>
                  <li><code style={{ color: '#91fff3' }}>Exp(x)</code> for exponential</li>
                  <li><code style={{ color: '#91fff3' }}>Log(x)</code> for natural logarithm</li>
                </ul>
              </div>

              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Examples
                </h4>
                <div style={{ 
                  backgroundColor: '#050505', 
                  padding: '16px', 
                  borderRadius: '4px',
                  border: '1px solid rgba(0, 191, 166, 0.2)',
                  marginTop: '12px'
                }}>
                  <p style={{ 
                    color: '#e8fff7', 
                    fontSize: '14px',
                    margin: '0 0 8px 0'
                  }}>
                    <strong style={{ color: '#00ffc3' }}>Single field:</strong>
                  </p>
                  <code style={{ 
                    color: '#91fff3', 
                    fontSize: '13px',
                    display: 'block',
                    marginBottom: '12px'
                  }}>
                    0.5 * m**2 * x(1)**2
                  </code>
                  
                  <p style={{ 
                    color: '#e8fff7', 
                    fontSize: '14px',
                    margin: '0 0 8px 0'
                  }}>
                    <strong style={{ color: '#00ffc3' }}>Two fields with parameters:</strong>
                  </p>
                  <code style={{ 
                    color: '#91fff3', 
                    fontSize: '13px',
                    display: 'block',
                    marginBottom: '12px'
                  }}>
                    A + G * x(1) - B * Tanh(c * x(1))
                  </code>
                  
                  <p style={{ 
                    color: '#e8fff7', 
                    fontSize: '14px',
                    margin: '0 0 8px 0'
                  }}>
                    <strong style={{ color: '#00ffc3' }}>With trigonometric functions:</strong>
                  </p>
                  <code style={{ 
                    color: '#91fff3', 
                    fontSize: '13px',
                    display: 'block'
                  }}>
                    lambda * (1 - Cos(x(1) / f))
                  </code>
                </div>
              </div>

              <div style={{ marginBottom: '24px' }}>
                <h4 style={{ 
                  color: '#00ffc3', 
                  fontSize: '16px', 
                  marginBottom: '12px',
                  marginTop: '0'
                }}>
                  Parameters
                </h4>
                <p style={{ 
                  color: '#e8fff7', 
                  fontSize: '14px',
                  lineHeight: '1.8',
                  margin: '0'
                }}>
                  You can use parameters (like <code style={{ color: '#91fff3' }}>m</code>, <code style={{ color: '#91fff3' }}>lambda</code>, <code style={{ color: '#91fff3' }}>A</code>, etc.) in your potential expression. 
                  Define these parameters in the "Model setup" page by specifying their names and values. 
                  The app will automatically replace the parameter symbols with their numeric values before execution.
                </p>
              </div>
            </div>
          </section>
        )}

        {/* ================== PAGE: MODEL SETUP ================== */}
        {activePage === "model" && (
          <section className="section">
            <h2 className="section-title">1. Model setup</h2>
            <p className="section-text">
              Choose the number of scalar fields and specify the potential
              V(φᵢ). You can use a simple math-like syntax (for example{" "}
              <code>0.5*m^2*x(1)**2 + lambda*x(1)**4</code>).
            </p>

            <div className="form-grid">
              {/* Number of fields input */}
              <label className="form-field">
                <span className="field-label">Number of fields</span>
                <input
                  type="text"
                  value={numFieldsInput}
                  onChange={handleNumFieldsChange}
                  onBlur={(e) => {
                    // On blur, validate and fix if needed
                    const n = parseInt(e.target.value, 10);
                    if (isNaN(n) || n < 1) {
                      setNumFieldsInput("1");
                      setNumFields(1);
                    } else {
                      setNumFieldsInput(String(n));
                      setNumFields(n);
                    }
                  }}
                  placeholder="e.g. 1, 2, 3"
                />
              </label>

              {/* Number of parameters input */}
              <label className="form-field">
                <span className="field-label">Number of parameters</span>
                <input
                  type="text"
                  value={numParametersInput}
                  onChange={handleNumParametersChange}
                  onBlur={(e) => {
                    // On blur, validate and fix if needed
                    const n = parseInt(e.target.value, 10);
                    if (isNaN(n) || n < 0) {
                      setNumParametersInput("0");
                      setNumParameters(0);
                    } else {
                      setNumParametersInput(String(n));
                      setNumParameters(n);
                    }
                  }}
                  placeholder="e.g. 0, 1, 2"
                />
              </label>

              {/* Potential input */}
              <label className="form-field form-field-full">
                <span className="field-label">Potential V(φ)</span>
                <textarea
                  rows={4}
                  value={potential}
                  onChange={(event) => setPotential(event.target.value)}
                  placeholder="Example: 0.5*m^2*phi1^2 + lambda*phi1^4"
                />
              </label>
            </div>

            {/* Metric table */}
            <div style={{ marginTop: "20px" }}>
              <h3 style={{ 
                color: "#00ffc3", 
                fontSize: "18px", 
                marginBottom: "12px" 
              }}>
                Metric
              </h3>
              <p className="section-text" style={{ marginBottom: "12px" }}>
                Define the metric matrix in field space.
               
              </p>
              <div style={{ 
                overflowX: "auto",
                marginTop: "12px",
                border: "1px solid rgba(0, 191, 166, 0.3)",
                borderRadius: "4px",
                padding: "16px",
                backgroundColor: "#0a0a0a"
              }}>
                <table style={{ 
                  width: "100%", 
                  borderCollapse: "collapse",
                  minWidth: numFields > 2 ? "400px" : "auto"
                }}>
                  <thead>
                    <tr>
                      <th style={{ 
                        padding: "8px", 
                        color: "#00ffc3", 
                        borderBottom: "1px solid rgba(0, 191, 166, 0.3)",
                        textAlign: "left"
                      }}></th>
                      {Array.from({ length: numFields }).map((_, j) => (
                        <th key={j} style={{ 
                          padding: "8px", 
                          color: "#00ffc3", 
                          borderBottom: "1px solid rgba(0, 191, 166, 0.3)",
                          textAlign: "center"
                        }}>
                          Field {j + 1}
                        </th>
                      ))}
                    </tr>
                  </thead>
                  <tbody>
                    {Array.from({ length: numFields }).map((_, i) => (
                      <tr key={i}>
                        <td style={{ 
                          padding: "8px", 
                          color: "#00ffc3", 
                          fontWeight: "bold",
                          borderRight: "1px solid rgba(0, 191, 166, 0.3)"
                        }}>
                          Field {i + 1}
                        </td>
                        {Array.from({ length: numFields }).map((_, j) => (
                          <td key={j} style={{ padding: "4px" }}>
                            <input
                              type="text"
                              value={metric[i] && metric[i][j] !== undefined ? metric[i][j] : (i === j ? "1.0" : "0.0")}
                              onChange={(event) => handleMetricChange(i, j, event)}
                              style={{
                                width: "100%",
                                padding: "6px",
                                backgroundColor: "#050505",
                                border: "1px solid rgba(0, 191, 166, 0.3)",
                                borderRadius: "2px",
                                color: "#e8fff7",
                                fontSize: "14px",
                                textAlign: "center"
                              }}
                              placeholder={i === j ? "1.0  (or x(1)**2)" : "0.0"}
                            />
                          </td>
                        ))}
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>

            {/* Parameters table */}
            {numParameters > 0 && (
              <div style={{ marginTop: "20px" }}>
                <h3 style={{ 
                  color: "#00ffc3", 
                  fontSize: "18px", 
                  marginBottom: "12px" 
                }}>
                  Parameters
                </h3>
                <p className="section-text" style={{ marginBottom: "12px" }}>
                  Define the parameter names and their values. Default value is 0.
                </p>
                <div className="fields-table">
                  {/* Header row */}
                  <div className="fields-header">
                    <span>Parameter</span>
                    <span>Name</span>
                    <span>Value</span>
                  </div>

                  {/* Rows for each parameter */}
                  {Array.from({ length: numParameters }).map((_, index) => (
                    <div className="fields-row" key={index}>
                      {/* Parameter label */}
                      <span className="field-name">Param {index + 1}</span>

                      {/* Parameter name input */}
                      <input
                        type="text"
                        value={parameters[index]?.name || ""}
                        onChange={(event) =>
                          handleParameterNameChange(index, event)
                        }
                        placeholder="e.g. m, lambda"
                      />

                      {/* Parameter value input */}
                      <input
                        type="text"
                        value={parameters[index]?.value || ""}
                        onChange={(event) =>
                          handleParameterValueChange(index, event)
                        }
                        placeholder="0"
                      />
                    </div>
                  ))}
                </div>
              </div>
            )}

            {/* Navigation buttons */}
            <div style={{ display: 'flex', justifyContent: 'space-between', marginTop: '30px', gap: '10px' }}>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("about")}
                style={{ 
                  backgroundColor: 'transparent',
                  color: '#00ffc3',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: '1px solid rgba(0, 191, 166, 0.5)',
                  cursor: 'pointer',
                  borderRadius: '8px',
                  transition: 'all 0.2s ease'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = 'rgba(0, 191, 166, 0.1)';
                  e.target.style.borderColor = '#00ffc3';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = 'transparent';
                  e.target.style.borderColor = 'rgba(0, 191, 166, 0.5)';
                }}
              >
                ← Back
              </button>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("initial")}
                style={{ 
                  backgroundColor: '#00ffc3',
                  color: '#0a0a0a',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: 'none',
                  cursor: 'pointer',
                  borderRadius: '8px',
                  transition: 'all 0.2s ease'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = '#00e5b3';
                  e.target.style.transform = 'translateY(-1px)';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = '#00ffc3';
                  e.target.style.transform = 'translateY(0)';
                }}
              >
                Next →
              </button>
            </div>
          </section>
        )}

        {/* ================== PAGE: INITIAL CONDITIONS ================== */}
        {activePage === "initial" && (
          <section className="section">
            <h2 className="section-title">2. Initial conditions</h2>
            <p className="section-text">
              Set the background initial conditions for each field and its
              conjugate momentum / time derivative.
            </p>

            {/* Initial time */}
            <div className="form-grid">
              <label className="form-field">
                <span className="field-label">Initial time</span>
                <input
                  type="text"
                  value={initialTime}
                  onChange={(event) => setInitialTime(event.target.value)}
                  placeholder="e.g. N = 0 or t = 0"
                />
              </label>
            </div>

            {/* Table for fields */}
            <div className="fields-table">
              {/* Header row */}
              <div className="fields-header">
                <span>Field</span>
                <span>ϕᵢ (initial value)</span>
                <span>ϕ̇ᵢ or πᵢ (initial velocity)</span>
              </div>

              {/* Rows for each field (from 1 to numFields) */}
              {Array.from({ length: numFields }).map((_, index) => (
                <div className="fields-row" key={index}>
                  {/* Field label φ1, φ2, ... */}
                  <span className="field-name">φ{index + 1}</span>

                  {/* Initial value input */}
                  <input
                    type="text"
                    value={initialValues[index] || ""}
                    onChange={(event) =>
                      handleFieldValueChange(index, event)
                    }
                    placeholder="e.g. 5.0"
                  />

                  {/* Initial velocity input */}
                  <input
                    type="text"
                    value={initialVelocities[index] || ""}
                    onChange={(event) =>
                      handleFieldVelocityChange(index, event)
                    }
                    placeholder="e.g. 0.0"
                  />
                </div>
              ))}
            </div>

            {/* Navigation buttons */}
            <div style={{ display: 'flex', justifyContent: 'space-between', marginTop: '30px', gap: '10px' }}>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("model")}
                style={{ 
                  backgroundColor: 'transparent',
                  color: '#00ffc3',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: '1px solid rgba(0, 191, 166, 0.5)',
                  cursor: 'pointer',
                  borderRadius: '8px',
                  transition: 'all 0.2s ease'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = 'rgba(0, 191, 166, 0.1)';
                  e.target.style.borderColor = '#00ffc3';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = 'transparent';
                  e.target.style.borderColor = 'rgba(0, 191, 166, 0.5)';
                }}
              >
                ← Back
              </button>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("summary")}
                style={{ 
                  backgroundColor: '#00ffc3',
                  color: '#0a0a0a',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px',
                  border: 'none',
                  cursor: 'pointer',
                  borderRadius: '8px',
                  transition: 'all 0.2s ease'
                }}
                onMouseEnter={(e) => {
                  e.target.style.backgroundColor = '#00e5b3';
                  e.target.style.transform = 'translateY(-1px)';
                }}
                onMouseLeave={(e) => {
                  e.target.style.backgroundColor = '#00ffc3';
                  e.target.style.transform = 'translateY(0)';
                }}
              >
                Next →
              </button>
            </div>
          </section>
        )}

        {/* ================== PAGE: SUMMARY / EXPORT ================== */}
        {activePage === "summary" && (
          <section className="section">
            <h2 className="section-title">3. Summary / Export</h2>
            <p className="section-text">
              Here is a quick preview of the configuration that will be sent to
               run.
            </p>

            {/* Show all data in JSON format (potential excluded to avoid duplication) */}
            <div style={{ marginTop: '16px', padding: '20px', backgroundColor: '#0a0a0a', borderRadius: '4px', border: '1px solid rgba(0, 191, 166, 0.3)', maxWidth: '95%' }}>
              <strong style={{ color: '#00ffc3', display: 'block', marginBottom: '12px', fontSize: '16px' }}>Configuration Summary:</strong>
              <pre style={{ 
                color: '#91fff3', 
                margin: 0, 
                fontSize: '12px',
                fontFamily: 'monospace',
                whiteSpace: 'pre-wrap',
                wordBreak: 'break-word',
                maxHeight: '200px',
                overflow: 'auto'
              }}>
{JSON.stringify(
  {
    numFields: numFields,
    numParameters: numParameters,
    parameters: parameters,
    initialTime: initialTime,
    initialValues: initialValues,
    initialVelocities: initialVelocities,
  },
  null,
  2
)}
              </pre>
            </div>
            
            {/* Show potential separately if it exists */}
            {potential && potential.trim() !== "" && (
              <div style={{ marginTop: '20px', padding: '20px', backgroundColor: '#0a0a0a', borderRadius: '4px', border: '1px solid rgba(0, 191, 166, 0.3)', maxWidth: '95%' }}>
                <strong style={{ color: '#00ffc3', display: 'block', marginBottom: '12px', fontSize: '16px' }}>Potential Expression:</strong>
                <pre style={{ 
                  color: '#91fff3', 
                  margin: 0, 
                  fontSize: '12px',
                  fontFamily: 'monospace',
                  whiteSpace: 'pre-wrap',
                  wordBreak: 'break-word'
                }}>
                  {potential}
                </pre>
              </div>
            )}

            <div style={{ display: 'flex', flexDirection: 'column', marginTop: '20px', gap: '15px' }}>
              {/* Buttons row */}
              <div style={{ display: 'flex', justifyContent: 'space-between', gap: '10px' }}>
                <div style={{ display: 'flex', gap: '10px' }}>
                  <button 
                    className="primary-button" 
                    onClick={() => setActivePage("initial")}
                    disabled={isCalculating}
                    style={{ 
                      backgroundColor: 'transparent',
                      color: '#00ffc3',
                      fontWeight: 'bold',
                      padding: '12px 24px',
                      fontSize: '16px',
                      border: '1px solid rgba(0, 191, 166, 0.5)',
                      cursor: isCalculating ? 'not-allowed' : 'pointer',
                      borderRadius: '8px',
                      transition: 'all 0.2s ease',
                      opacity: isCalculating ? 0.5 : 1
                    }}
                    onMouseEnter={(e) => {
                      if (!isCalculating) {
                        e.target.style.backgroundColor = 'rgba(0, 191, 166, 0.1)';
                        e.target.style.borderColor = '#00ffc3';
                      }
                    }}
                    onMouseLeave={(e) => {
                      if (!isCalculating) {
                        e.target.style.backgroundColor = 'transparent';
                        e.target.style.borderColor = 'rgba(0, 191, 166, 0.5)';
                      }
                    }}
                  >
                    ← Back
                  </button>
                </div>
                <div style={{ display: 'flex', gap: '10px' }}>
                  {isCalculating && (
                    <button 
                      className="primary-button" 
                      onClick={handleCancelCalculation}
                      style={{
                        backgroundColor: 'transparent',
                        color: '#ff4444',
                        fontWeight: 'bold',
                        padding: '12px 24px',
                        fontSize: '16px',
                        border: '1px solid #ff4444',
                        cursor: 'pointer',
                        borderRadius: '8px',
                        transition: 'all 0.2s ease'
                      }}
                      onMouseEnter={(e) => {
                        e.target.style.backgroundColor = 'rgba(255, 68, 68, 0.1)';
                        e.target.style.borderColor = '#ff3333';
                        e.target.style.color = '#ff3333';
                        e.target.style.transform = 'translateY(-1px)';
                      }}
                      onMouseLeave={(e) => {
                        e.target.style.backgroundColor = 'transparent';
                        e.target.style.borderColor = '#ff4444';
                        e.target.style.color = '#ff4444';
                        e.target.style.transform = 'translateY(0)';
                      }}
                    >
                      Cancel Calculation
                    </button>
                  )}
                  {!isCalculating && (
                    <button 
                      className="primary-button" 
                      onClick={handleRunCalculation}
                      style={{
                        backgroundColor: '#4CAF50',
                        cursor: 'pointer',
                        borderRadius: '8px',
                        color: '#fff',
                        fontWeight: 'bold',
                        padding: '12px 24px',
                        fontSize: '16px',
                        border: 'none',
                        transition: 'all 0.2s ease'
                      }}
                      onMouseEnter={(e) => {
                        e.target.style.backgroundColor = '#45a049';
                        e.target.style.transform = 'translateY(-1px)';
                      }}
                      onMouseLeave={(e) => {
                        e.target.style.backgroundColor = '#4CAF50';
                        e.target.style.transform = 'translateY(0)';
                      }}
                    >
                      Run Calculation
                    </button>
                  )}
                </div>
              </div>
              
              {/* Calculating status indicator (not a button) */}
              {isCalculating && (
                <div style={{
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  padding: '12px 24px',
                  color: '#00ffc3',
                  fontSize: '16px',
                  fontWeight: '500',
                  fontStyle: 'italic'
                }}>
                  <span style={{ 
                    marginRight: '8px',
                    animation: 'pulse 1.5s ease-in-out infinite'
                  }}>⏳</span>
                  Calculating...
                </div>
              )}
            </div>
            
            {/* Display calculation results */}
            {error && !error.includes("cancelled") && (
              <div className="summary-block" style={{ 
                marginTop: '20px', 
                borderColor: '#ff4444',
                color: '#ff4444'
              }}>
                <strong>Error:</strong> {error}
              </div>
            )}
            
            {/* Don't show result panel if calculation was cancelled */}
            {calculationResult && !calculationResult.message?.toLowerCase().includes("cancelled") && (
              <div className="summary-block" style={{ 
                marginTop: '30px',
                width: '100%',
                maxWidth: '98%',
                padding: '40px',
                minHeight: '600px'
              }}>
                <h3 style={{ 
                  marginTop: '0', 
                  marginBottom: '20px',
                  color: calculationResult.success ? '#00ffc3' : '#ff4444',
                  fontSize: '28px'
                }}>
                  {calculationResult.success ? '✓' : '✗'} Calculation Result
                </h3>
                
                <p style={{ marginBottom: '12px', color: '#e8fff7', fontSize: '15px' }}>
                  <strong>Status:</strong> <span style={{ color: calculationResult.success ? '#00ffc3' : '#ff4444' }}>
                    {calculationResult.success ? 'Success' : 'Failed'}
                  </span>
                </p>
                
                <p style={{ marginBottom: '12px', color: '#e8fff7', fontSize: '15px' }}>
                  <strong>Message:</strong> {calculationResult.message}
                </p>
                
                {calculationResult.executionId && (
                  <p style={{ marginBottom: '12px', fontSize: '13px', color: '#91fff3', opacity: 0.7 }}>
                    <strong>Execution ID:</strong> {calculationResult.executionId.substring(0, 8)}...
                  </p>
                )}
                
                {calculationResult.success && calculationResult.outputFiles && calculationResult.outputFiles.length > 0 && (
                  <div style={{ marginTop: '16px', paddingTop: '16px', borderTop: '1px solid rgba(0, 191, 166, 0.3)' }}>
                    <strong style={{ color: '#00ffc3', fontSize: '16px' }}>Output Files ({calculationResult.outputFiles.length}):</strong>
                    <ul style={{ marginTop: '10px', marginBottom: '0', paddingLeft: '20px', color: '#e8fff7', fontSize: '14px' }}>
                      {calculationResult.outputFiles.map((file, idx) => {
                        const isImage = file.toLowerCase().endsWith('.png') || file.toLowerCase().endsWith('.jpg') || file.toLowerCase().endsWith('.jpeg');
                        // Add cache-busting query parameter to force browser to reload the image
                        // Use executionId and timestamp to ensure unique URL for each calculation
                        // For plot files, use a more aggressive cache-busting approach
                        const isPlotFile = file.toLowerCase().includes('plot');
                        const cacheBuster = calculationResult.executionId 
                          ? `?t=${Date.now()}&id=${calculationResult.executionId.substring(0, 8)}&v=${isPlotFile ? 'plot' : 'file'}`
                          : `?t=${Date.now()}&v=${isPlotFile ? 'plot' : 'file'}`;
                        const fileUrl = `${API_BASE_URL}/api/cosmo-perturbations/files/${encodeURIComponent(file)}${cacheBuster}`;
                        
                        return (
                          <li key={idx} style={{ marginBottom: '8px' }}>
                            {isImage ? (
                              <div style={{ 
                                width: '100%', 
                                maxWidth: '600px',
                                marginTop: '20px',
                                marginBottom: '20px'
                              }}>
                                <img 
                                  src={fileUrl} 
                                  alt={file}
                                  key={`${file}-${calculationResult.executionId || Date.now()}`}
                                  style={{
                                    width: '100%',
                                    height: 'auto',
                                    maxHeight: '400px',
                                    border: '1px solid rgba(0, 191, 166, 0.3)',
                                    borderRadius: '4px',
                                    backgroundColor: '#ffffff',
                                    boxShadow: '0 2px 8px rgba(0, 191, 166, 0.2)',
                                    display: 'block',
                                    cursor: 'pointer'
                                  }}
                                  onClick={() => window.open(fileUrl, '_blank')}
                                  onError={(e) => {
                                    e.target.style.display = 'none';
                                    console.error('Failed to load image:', fileUrl);
                                  }}
                                  onLoad={() => {
                                    // Force image refresh by removing and re-adding cache-busting parameter
                                    console.log('Plot image loaded:', file);
                                  }}
                                  loading="eager"
                                />
                              </div>
                            ) : (
                              <a 
                                href={fileUrl} 
                                target="_blank" 
                                rel="noopener noreferrer"
                                style={{ color: '#00ffc3', textDecoration: 'underline' }}
                              >
                                {file}
                              </a>
                            )}
                          </li>
                        );
                      })}
                    </ul>
                  </div>
                )}
                
                {calculationResult.output && (
                  <details style={{ marginTop: '12px', paddingTop: '12px', borderTop: '1px solid rgba(0, 191, 166, 0.3)' }}>
                    <summary style={{ 
                      cursor: 'pointer', 
                      color: '#00ffc3',
                      fontSize: '13px'
                    }}>
                      View Output
                    </summary>
                    <pre style={{ 
                      backgroundColor: '#0a0a0a', 
                      color: '#91fff3',
                      padding: '12px', 
                      overflow: 'auto', 
                      maxHeight: '400px',
                      borderRadius: '4px',
                      marginTop: '8px',
                      fontSize: '11px',
                      fontFamily: 'monospace',
                      border: '1px solid rgba(0, 191, 166, 0.3)'
                    }}>
                      {calculationResult.output}
                    </pre>
                  </details>
                )}
              </div>
            )}
            <p className="hint-text">
              Right now it works  without change of metric. For more fields, the code maybe be crash because of server limitations.
              Later these issues will be fixed. 
            </p>
          </section>
        )}

        {/* Footer */}
        <footer style={{ 
          textAlign: "center", 
          marginTop: "auto", 
          paddingTop: "40px",
          fontSize: "18px"
        }}>
          <span style={{ color: "#00F5C4" }}>Created By{" "}</span>
          <a 
            href="https://ioanna-stamou.com/" 
            target="_blank" 
            rel="noopener noreferrer"
            style={{
              color: "#00F5C4",
              textDecoration: "none",
              transition: "opacity 0.2s ease"
            }}
            onMouseEnter={(e) => e.target.style.opacity = "0.7"}
            onMouseLeave={(e) => e.target.style.opacity = "1"}
          >
            Ioanna Stamou
          </a>
        </footer>

      </main>
    </div>
  );
}

// We export the App component so main.jsx can render it.
export default App;
