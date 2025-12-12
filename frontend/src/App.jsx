// src/App.jsx
// We import React's useState so we can store changing values.
import { useState } from "react";
import "./App.css";

function App() {
  // Which page is currently visible: "model", "initial", "summary", or "about"
  const [activePage, setActivePage] = useState("about");
  // Number of scalar fields
  const [numFields, setNumFields] = useState(1);
  // Potential V(phi)
  const [potential, setPotential] = useState("0.5 * m^2 * phi1^2");
  // Arrays for initial values and velocities of each field
  const [initialValues, setInitialValues] = useState([""]);
  const [initialVelocities, setInitialVelocities] = useState([""]);
  // Initial time (for example N = 0 or t = 0)
  const [initialTime, setInitialTime] = useState("0.0");
  // Number of parameters
  const [numParameters, setNumParameters] = useState(0);
  // Array of parameter objects: [{name: "m", value: "1.0"}, ...]
  const [parameters, setParameters] = useState([]);

  // ----------------------------------------------------
  //  update number of fields AND keep arrays in sync
  // ----------------------------------------------------
  function handleNumFieldsChange(event) {
    // event.target.value is a string from the input
    const valueAsString = event.target.value;

    // Convert to integer
    let n = parseInt(valueAsString, 10);

    // If conversion fails or n < 1, set n = 1
    if (isNaN(n) || n < 1) {
      n = 1;
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
    let n = parseInt(valueAsString, 10);
    
    // If conversion fails or n < 0, set n = 0
    if (isNaN(n) || n < 0) {
      n = 0;
    }
    
    setNumParameters(n);
    
    // Update parameters array to have length n
    setParameters((previousArray) => {
      const newArray = [...previousArray];
      
      if (n > newArray.length) {
        // If we need more parameters, add empty ones with default value "0"
        while (newArray.length < n) {
          newArray.push({ name: "", value: "0" });
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
    copy[index] = { ...copy[index], value: newValue || "0" };
    setParameters(copy);
  }

  // ----------------------------------------------------
  // State for calculation results
  // ----------------------------------------------------
  const [calculationResult, setCalculationResult] = useState(null);
  const [isCalculating, setIsCalculating] = useState(false);
  const [error, setError] = useState(null);

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
    setCalculationResult(null);
    
    // Prepare parameters object from the parameters array
    const parametersObj = {};
    parameters.forEach(param => {
      if (param.name && param.name.trim() !== "") {
        const value = parseFloat(param.value);
        parametersObj[param.name] = isNaN(value) ? 0 : value;
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
      parameters: parametersObj,
      numParameters: numParameters
    };
    
    try {
      const response = await fetch('/api/cosmo-perturbations/calculate', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(requestData)
      });
      
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      
      const result = await response.json();
      setCalculationResult(result);
      
      if (result.success) {
        const fileCount = result.outputFiles ? result.outputFiles.length : 0;
        alert(`✅ Calculation completed successfully!\n\nGenerated ${fileCount} output file(s).\n\nCheck the results below for details.`);
      } else {
        alert("❌ Calculation failed: " + result.message + "\n\nCheck the error details below.");
      }
    } catch (err) {
      setError(err.message);
      alert("Error: " + err.message);
      console.error("Error:", err);
    } finally {
      setIsCalculating(false);
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
              objectFit: "contain"
            }}
          />
        </div>
        <div className="menu-title">Menu</div>

        <nav className="menu-list">

           {/* Button: About */}
          <button
            className={`menu-item ${activePage === "about" ? "active" : ""}`}
            onClick={() => setActivePage("about")}
          >
            About
          </button>

          {/* Button: Model setup */}
          <button
            className={`menu-item ${activePage === "model" ? "active" : ""}`}
            onClick={() => setActivePage("model")}
          >
            Model setup
          </button>

          {/* Button: Initial conditions */}
          <button
            className={`menu-item ${activePage === "initial" ? "active" : ""}`}
            onClick={() => setActivePage("initial")}
          >
            Initial conditions
          </button>

          {/* Button: Summary / Export */}
          <button
            className={`menu-item ${activePage === "summary" ? "active" : ""}`}
            onClick={() => setActivePage("summary")}
          >
            Summary / Export
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
            wiil give you the power spectra and observables.
          </p>
        </section>

        {/* ================== PAGE: ABOUT ================== */}
        {activePage === "about" && (
          <section className="section">
            <h2 className="section-title">About this app</h2>
            <p className="section-text">
              This app provides the cosmological perturbations 
              for a given inflationary potential and initial condition. The idea is:
            </p>
            <ol className="section-list">
              <li>Define the inflationary potential and model parameters.</li>
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
          </section>
        )}

        {/* ================== PAGE: MODEL SETUP ================== */}
        {activePage === "model" && (
          <section className="section">
            <h2 className="section-title">1. Model setup</h2>
            <p className="section-text">
              Choose the number of scalar fields and specify the potential
              V(φᵢ). You can use a simple math-like syntax (for example{" "}
              <code>0.5*m^2*phi1^2 + lambda*phi1^4</code>).
            </p>

            <div className="form-grid">
              {/* Number of fields input */}
              <label className="form-field">
                <span className="field-label">Number of fields</span>
                <input
                  type="number"
                  min="1"
                  value={numFields}
                  onChange={handleNumFieldsChange}
                />
              </label>

              {/* Number of parameters input */}
              <label className="form-field">
                <span className="field-label">Number of parameters</span>
                <input
                  type="number"
                  min="0"
                  value={numParameters}
                  onChange={handleNumParametersChange}
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
                        value={parameters[index]?.value || "0"}
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

            {/* Next button */}
            <div style={{ display: 'flex', justifyContent: 'flex-end', marginTop: '30px' }}>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("initial")}
                style={{ 
                  backgroundColor: '#00ffc3',
                  color: '#0a0a0a',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px'
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
                    placeholder="e.g. 15.0"
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

            {/* Next button */}
            <div style={{ display: 'flex', justifyContent: 'flex-end', marginTop: '30px' }}>
              <button 
                className="primary-button" 
                onClick={() => setActivePage("summary")}
                style={{ 
                  backgroundColor: '#00ffc3',
                  color: '#0a0a0a',
                  fontWeight: 'bold',
                  padding: '12px 24px',
                  fontSize: '16px'
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
              the backend solver.
            </p>

            {/* Show all data in JSON format */}
            <pre className="summary-block">
{JSON.stringify(
  {
    numFields: numFields,
    potential: potential,
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

            <div style={{ display: 'flex', justifyContent: 'flex-end', marginTop: '20px' }}>
              <button 
                className="primary-button" 
                onClick={handleRunCalculation}
                disabled={isCalculating}
                style={{ 
                  backgroundColor: isCalculating ? '#ccc' : '#4CAF50',
                  cursor: isCalculating ? 'not-allowed' : 'pointer'
                }}
              >
                {isCalculating ? 'Calculating...' : 'Run Calculation'}
              </button>
            </div>
            
            {/* Display calculation results */}
            {error && (
              <div className="summary-block" style={{ 
                marginTop: '20px', 
                borderColor: '#ff4444',
                color: '#ff4444'
              }}>
                <strong>Error:</strong> {error}
              </div>
            )}
            
            {calculationResult && (
              <div className="summary-block" style={{ marginTop: '20px' }}>
                <h3 style={{ 
                  marginTop: '0', 
                  marginBottom: '12px',
                  color: calculationResult.success ? '#00ffc3' : '#ff4444',
                  fontSize: '18px'
                }}>
                  {calculationResult.success ? '✓' : '✗'} Calculation Result
                </h3>
                
                <p style={{ marginBottom: '8px', color: '#e8fff7' }}>
                  <strong>Status:</strong> <span style={{ color: calculationResult.success ? '#00ffc3' : '#ff4444' }}>
                    {calculationResult.success ? 'Success' : 'Failed'}
                  </span>
                </p>
                
                <p style={{ marginBottom: '8px', color: '#e8fff7' }}>
                  <strong>Message:</strong> {calculationResult.message}
                </p>
                
                {calculationResult.executionId && (
                  <p style={{ marginBottom: '8px', fontSize: '12px', color: '#91fff3', opacity: 0.7 }}>
                    <strong>Execution ID:</strong> {calculationResult.executionId.substring(0, 8)}...
                  </p>
                )}
                
                {calculationResult.success && calculationResult.outputFiles && calculationResult.outputFiles.length > 0 && (
                  <div style={{ marginTop: '12px', paddingTop: '12px', borderTop: '1px solid rgba(0, 191, 166, 0.3)' }}>
                    <strong style={{ color: '#00ffc3' }}>Output Files ({calculationResult.outputFiles.length}):</strong>
                    <ul style={{ marginTop: '6px', marginBottom: '0', paddingLeft: '20px', color: '#e8fff7', fontSize: '13px' }}>
                      {calculationResult.outputFiles.map((file, idx) => (
                        <li key={idx} style={{ marginBottom: '4px' }}>
                          {file}
                        </li>
                      ))}
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
                      maxHeight: '200px',
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
              Later, this button will send the data to your Fortran backend and
              show power spectra / observables here.
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
