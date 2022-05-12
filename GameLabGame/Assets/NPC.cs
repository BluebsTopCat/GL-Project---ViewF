using System;
using System.Collections;
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;
using Yarn.Unity;


public class NPC : MonoBehaviour
{
    [Header("Important")]
    private DialogueRunner dr;
    private GameObject player;
    [Header("InteractDialogue")] 
    public string InterDialogue;
    [Header("Trigger Dialogue")]
    public bool trigger;
    public BoxCollider bc;
    public string TriggerDialogue;
    public bool once;
    private bool interacted = false;
    // Start is called before the first frame update
    void Start()
    {
        if(trigger)
            bc = this.GetComponent<BoxCollider>();
        dr = GameObject.FindObjectOfType<DialogueRunner>();
        player = GameObject.FindWithTag("Player");
    }


    public string Interact()
    {
        return(InterDialogue);
    }
    private void OnTriggerEnter(Collider other)
    {
        if (trigger && other.gameObject == player && (!interacted || !once)) 
        {
            interacted = true;
            dr.StartDialogue(TriggerDialogue);
        }
    }
}
