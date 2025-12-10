// src/App.jsx
import { useState } from "react";
import "./App.css";

function App() {
  const [activePage, setActivePage] = useState("model");

  // Core physics inputs
  const [numFields, setNumFields] = useState(1);
  const [potential, setPotential] = useState("0.5 * m^2 * phi1^2");
  const [initialValues, setInitialValues] = useState([""]);
  const [initialVelocities, setInitialVelocities] = useState([""]);
  const [initialTime, setInitialTime] = useState("0.0");

  // Whenever numFields changes, keep arrays in sync
  const handleNumFieldsChange = (value) => {
    const n = Math.max(1, Number(value) || 1);
    setNumFields(n);

    setInitialValues((old) => {
      const copy = [...old];
      copy.length = n;
      return copy.map((v) => v ?? "");
    });

    setInitialVelocities((old) => {
      const copy = [...old];
      copy.length = n;
      return copy.map((v) => v ?? "");
    });
  };

  const handleFieldValueChange = (index, value) => {
    const copy = [...initialValues];
    copy[index] = value;
    setInitialValues(copy);
  };

  const handleFieldVelocityChange = (index, value) => {
    const copy = [...initialVelocities];
    copy[index] = value;
    setInitialVelocities(copy);
  };

  // For now this just logs the configuration – later it will call your backend
  const handlePreviewConfig = () => {
    const config = {
      numFields,
      potential,
      initialTime,
      initialValues,
      initialVelocities,
    };

    console.log("Cosmological perturbation config:", config);
    alert("Parameters collected! (Next step: connect the Fortran backend.)");
  };

  return (
    <div className="app">
      {/* LEFT MENU */}
      <aside className="sidebar">
        <div className="menu-title">Menu</div>

        <nav className="menu-list">
          <button
            className={`menu-item ${activePage === "model" ? "active" : ""}`}
            onClick={() => setActivePage("model")}
          >
            Model setup
          </button>
          <button
            className={`menu-item ${activePage === "initial" ? "active" : ""}`}
            onClick={() => setActivePage("initial")}
          >
            Initial conditions
          </button>
          <button
            className={`menu-item ${activePage === "summary" ? "active" : ""}`}
            onClick={() => setActivePage("summary")}
          >
            Summary / Export
          </button>
          <button
            className={`menu-item ${activePage === "about" ? "active" : ""}`}
            onClick={() => setActivePage("about")}
          >
            About
          </button>
        </nav>
      </aside>

      {/* MAIN CONTENT */}
      <main className="content">
        {/* TOP BANNER */}
        <section className="header-banner">
          <h1 className="name-title">Cosmological Perturbations</h1>
          <p className="header-subtitle">
            Define your multi-field inflation model and initial conditions. The
            Fortran solver will be plugged in later.
          </p>
        </section>

        {/* DYNAMIC SECTIONS */}
        {activePage === "model" && (
          <section className="section">
            <h2 className="section-title">1. Model setup</h2>
            <p className="section-text">
              Choose the number of scalar fields and specify the potential
              \(V(\phi_i)\). You can use a simple math-like syntax (e.g.{" "}
              <code>0.5*m^2*phi1^2 + lambda*phi1^4</code>).
            </p>

            <div className="form-grid">
              <label className="form-field">
                <span className="field-label">Number of fields</span>
                <input
                  type="number"
                  min="1"
                  value={numFields}
                  onChange={(e) => handleNumFieldsChange(e.target.value)}
                />
              </label>

              <label className="form-field form-field-full">
                <span className="field-label">Potential V(φ)</span>
                <textarea
                  rows={4}
                  value={potential}
                  onChange={(e) => setPotential(e.target.value)}
                  placeholder="Example: 0.5*m^2*phi1^2 + lambda*phi1^4"
                />
              </label>
            </div>
          </section>
        )}

        {activePage === "initial" && (
          <section className="section">
            <h2 className="section-title">2. Initial conditions</h2>
            <p className="section-text">
              Set the background initial conditions for each field and its
              conjugate momentum / time derivative.
            </p>

            <div className="form-grid">
              <label className="form-field">
                <span className="field-label">Initial time</span>
                <input
                  type="text"
                  value={initialTime}
                  onChange={(e) => setInitialTime(e.target.value)}
                  placeholder="e.g. N = 0 or t = 0"
                />
              </label>
            </div>

            <div className="fields-table">
              <div className="fields-header">
                <span>Field</span>
                <span>ϕᵢ (initial value)</span>
                <span>ϕ̇ᵢ or πᵢ (initial velocity)</span>
              </div>

              {Array.from({ length: numFields }).map((_, i) => (
                <div className="fields-row" key={i}>
                  <span className="field-name">φ{i + 1}</span>
                  <input
                    type="text"
                    value={initialValues[i] || ""}
                    onChange={(e) =>
                      handleFieldValueChange(i, e.target.value)
                    }
                    placeholder="e.g. 15.0"
                  />
                  <input
                    type="text"
                    value={initialVelocities[i] || ""}
                    onChange={(e) =>
                      handleFieldVelocityChange(i, e.target.value)
                    }
                    placeholder="e.g. 0.0"
                  />
                </div>
              ))}
            </div>
          </section>
        )}

        {activePage === "summary" && (
          <section className="section">
            <h2 className="section-title">3. Summary / Export</h2>
            <p className="section-text">
              Here is a quick preview of the configuration that will be sent to
              the backend solver.
            </p>

            <pre className="summary-block">
{JSON.stringify(
  {
    numFields,
    potential,
    initialTime,
    initialValues,
    initialVelocities,
  },
  null,
  2
)}
            </pre>

            <button className="primary-button" onClick={handlePreviewConfig}>
              Preview configuration (console log)
            </button>
            <p className="hint-text">
              Later, this button will send the data to your Fortran backend and
              show power spectra / observables here.
            </p>
          </section>
        )}

        {activePage === "about" && (
          <section className="section">
            <h2 className="section-title">About this app</h2>
            <p className="section-text">
              This frontend is the first step towards a full cosmological
              perturbation pipeline. The idea is:
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
      </main>
    </div>
  );
}

export default App;
