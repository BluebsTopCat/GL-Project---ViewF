using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ocean : MonoBehaviour
{
    private Player p;

    private void Start()
    {
        p = GameObject.FindObjectOfType<Player>();
    }

    private void OnTriggerEnter(Collider other)
    {
        if (other.gameObject == p.gameObject)
        {
            p.drown();
        }
    }
}
