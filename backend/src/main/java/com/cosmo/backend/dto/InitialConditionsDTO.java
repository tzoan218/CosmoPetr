package com.cosmo.backend.dto;

import java.util.List;

//Data Transfer Object for initial conditions
//Used to receive data from frontend

public class InitialConditionsDTO {

   // initial fields values (phi, values) Example [6.33]:
    private List<Double> fieldValues;

   // initial field  velocities:
   private List<Double> fieldVelocities;

   //initial time:
    private Double initialTime;

    //timestep:
    private Double timeStep;

    //kstar parameter:
    private Double kstar;

    //cq parameter:
    private Double cq;

    // potential type:
    private String potentialType;


    //potential parameters:
    private List<Double> potentialParameters;


    //Potential expression
    private String potentialExpression;

    //Constructors
    public InitialConditionsDTO(){
        this.initialTime=0.0;
        this.timeStep=0.05;
        this.kstar =0.05;
        this.cq =100.0;
        //this.potentialType= ;
        //this.potetnialParameters= List.of(0.1,0.5,0.8,4,0.6)

    }


    //Getters and Setters
    public List<Double> getFieldValues(){
        return fieldValues;
    }

    public void setFieldValues (List<Double> fieldValue){
        this.fieldValues = fieldValue;
    }

    public List<Double> getFieldVelocities(){
        return fieldVelocities;
    }

    public void setFieldVelocities (List<Double> fieldVelocities){
        this.fieldVelocities=fieldVelocities;
    }

    public Double getInitialTime(){
        return initialTime;
    }

    public void setInitialTime(Double initialTime){
        this.initialTime = initialTime;
    }

    public Double getTimeStep(){
        return timeStep;
    }

    public void setTimeStep(Double timeStep){
        this.timeStep =timeStep;
    }

    public Double getKstar(){
        return kstar;
    }

    public void setKstar(Double kstar){
        this.kstar =kstar;
    }

    public Double getCq(){
        return cq;
    }

    public void setCq(Double cq){
        this.cq=cq;
    }

    public String getPotentialType() {
        return potentialType;
    }

    public void setPotentialType(String potentialType) {
        this.potentialType = potentialType;
    }

    public List<Double> getPotentialParameters() {
        return potentialParameters;
    }

    public void setPotentialParameters(List<Double> potentialParameters) {
        this.potentialParameters = potentialParameters;
    }

    public String getPotentialExpression() {
        return potentialExpression;
    }

    public void setPotentialExpression(String potentialExpression) {
        this.potentialExpression = potentialExpression;
    }
}
