using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(Camera))]
public class SkyboxCamera : MonoBehaviour
{
    public float skyboxScale = .05f;

    private PlayerController _player;
    private Camera _camera;
   

    // Update is called once per frame
    private void Start()
    {
        setup();
    }

    void Update()
    {
        if (_player == null)
        {
            setup();
            return;
        }

        Transform camTransform = _player.camera.transform;
        
        transform.localPosition = camTransform.position * skyboxScale;
        transform.rotation = camTransform.rotation;
    }

    void setup()
    {
        _player = PlayerController.Instance;
        _camera = this.GetComponent<Camera>();
        _camera.fieldOfView = _player.camera.fieldOfView;
    }
}
