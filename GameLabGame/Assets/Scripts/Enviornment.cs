using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Remoting.Messaging;
using UnityEngine;
using UnityEngine.Experimental.GlobalIllumination;
using UnityEngine.Tilemaps;

public class Enviornment : MonoBehaviour
{
    [Header("Testing Settings")]
    public float speeditup = 1;
    public float GlobalTime;
    [Range(0,1)]
    public float CloudColorStr = 0.446f;

    public float CloudSpeedMult = .05f;

    Vector2 cloudAcc;
    Vector2 cloudPos;
    Vector2 cloudVel;

    [Header("Properties")]
    public Light Sun;
    public Color Cloud_Color;
    public Gradient LowGrad;
    public Gradient HighGrad;
    
    // Start is called before the first frame update

    private void OnValidate()
    {
        UpdateShaders();
    }

    void UpdateShaders()
    {
        float standardTime = (GlobalTime % 24);
        
        Color skyTop = HighGrad.Evaluate(standardTime/ 24);
        
        Shader.SetGlobalColor("Sky_Top", skyTop);
        Shader.SetGlobalColor("Sky_Bot", LowGrad.Evaluate(standardTime/24));
        Shader.SetGlobalFloat("Time", standardTime);
        Shader.SetGlobalFloat("CloudCover", RandBetween(0,1, GlobalTime, .1f));
        Shader.SetGlobalFloat("CloudColorStr", CloudColorStr);
        Shader.SetGlobalColor("CloudColor", Cloud_Color);
        Shader.SetGlobalVector("CloudPos", cloudPos);
        Sun.color = skyTop;
        float sunRot = -90 + 360 * (standardTime / 24);
        Sun.transform.eulerAngles = new Vector3(sunRot, 0f, 0f);
    }


    // Update is called once per frame
    void Update()
    {
        GlobalTime += UnityEngine.Time.deltaTime * speeditup;
        
        cloudAcc.x = RandBetween(-1, 1, GlobalTime, .1f);
        cloudAcc.y = RandBetween(-1, 1, GlobalTime + 50, .1f);
        cloudVel += new Vector2(cloudAcc.x * Time.deltaTime, cloudAcc.y * Time.deltaTime);
        cloudPos += cloudVel * (Time.deltaTime * CloudSpeedMult);
        UpdateShaders();
    }

    private float RandBetween(float min, float max, float time, float speed)
    {
        float half = (max - min) / 2;
        float perlintime = (Mathf.Sin(2 * (time * speed)) + Mathf.Sin(Mathf.PI * (time * speed)))/2;
        return min + half + perlintime * half;
    }

    private float IntegralCalc(float min, float max, float time, float speed)
    {
        float half = (max - min) / 2;
        float pi = MathF.PI;
        float perlinintegral = (-Mathf.Cos(pi * speed * time)/pi - Mathf.Cos(2 * time * speed)/2) /2;
        return min + half + perlinintegral * half;
    }
}
