using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Remoting.Messaging;
using UnityEngine;
using UnityEngine.Experimental.GlobalIllumination;

public class Enviornment : MonoBehaviour
{
    [Header("Testing Settings")]
    [Range(0.0f,24f)]
    public float Time;

    [Header("Properties")]
    public Material Skybox;
    public Light Sun;
    public Gradient LowGrad;
    public Gradient HighGrad;
    
    // Start is called before the first frame update

    private void OnValidate()
    {
        float standardTime = (Time % 24);
        Color skyTop = HighGrad.Evaluate(standardTime/ 24);
        Shader.SetGlobalColor("Sky_Top", skyTop);
        Shader.SetGlobalColor("Sky_Bot", LowGrad.Evaluate(standardTime/24));
        Shader.SetGlobalFloat("Time", standardTime);

        Sun.color = skyTop;
        float sunRot = -90 + 360 * (Time / 24);
        Sun.transform.eulerAngles = new Vector3(sunRot, 0f, 0f);
    }

    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        
    }
}
