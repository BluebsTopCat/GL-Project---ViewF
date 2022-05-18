using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Yarn.Unity;

[RequireComponent(typeof(Rigidbody))]
[RequireComponent(typeof(Collider))]
[RequireComponent(typeof(Animator))]
public class Player : MonoBehaviour
{
    public enum Playerstate
    {
        Walking,
        Photoing,
        Talking
    };
    public Playerstate playerCurrently;
    [Header("Camera")] 
    public bool ineditor;
    public bool incamera;
    public GameObject cameraObject;
    public GameObject cameracam;
    public float playercammult;
    public GameObject photocanvas;

    [Header("Dialogue")] 
    public bool indialogue;
    private DialogueRunner dr;
    private DialogueUI dui;
    [Header("Move")] 
    public bool sprinting;
    public float sprintmultiplier;
    public float speed;
    public float groundheight = 1.5f;
    public float yadjstr = 100;
    public LayerMask groundlm;
    [Header("Look")] 
    public Camera cam;
    private float roty;
    private float rotx;

    [Header("Animation")] 
    private Animator anim;

    [Header("Dialogue")]
    public float InteractLength = 10;
    public LayerMask NPCMASK;
    
    [Header("Jumping")] 
    public float gravity;
    public float timefalling;
    public float jumpstr;
    public bool jumping;
    private Rigidbody rb;
    private RaycastHit ground;
    public Boolean grounded;
    public Boolean inadjustrange;

    // Start is called before the first frame update
    void Start()
    {
        rb = this.GetComponent<Rigidbody>();
        dr = GameObject.FindObjectOfType<DialogueRunner>();
        dui = GameObject.FindObjectOfType<DialogueUI>();
        anim = this.GetComponent<Animator>();
        Cursor.lockState = CursorLockMode.Locked;
    }

    // Update is called once per frame
    void Update()
    {
        indialogue = dr.IsDialogueRunning;
        //Check ground status
        if (indialogue)
        {
            playerCurrently = Playerstate.Talking;
            Cursor.lockState = CursorLockMode.Confined;
            Cursor.visible = true;
        }
        else if (ineditor)
        {
            playerCurrently = Playerstate.Photoing;
            Cursor.lockState = CursorLockMode.Confined;
            Cursor.visible = true;
        }
        else
        {
            playerCurrently = Playerstate.Walking;
            Cursor.lockState = CursorLockMode.Locked;
        }

        if (playerCurrently == Playerstate.Photoing)
        {
            Time.timeScale = 0;
            rb.isKinematic = true;
            return;
        }

        photocanvas.SetActive(false);
        Time.timeScale = 1;
        rb.isKinematic = false;
        inadjustrange = Physics.Raycast(transform.position, Vector3.down,out ground, groundheight * 1.25f, groundlm);
        grounded = Vector3.Distance(ground.point, transform.position) <= groundheight +.01f;

        if (grounded || rb.velocity.y <= 0)
            jumping = false;

        
        if(playerCurrently == Playerstate.Talking){
            jumping = false;
            if(Input.GetKeyDown(KeyCode.Space))
                dui.MarkLineComplete();
            if(inadjustrange)
                adjusty();
            
            rb.velocity = Vector3.Lerp(rb.velocity, new Vector3(0, 0f, 0),.5f);
            anim.SetFloat("Speed", 0);
            cameraObject.SetActive(false);
            return;
        }
        
        
        
        
        timefalling = grounded || (inadjustrange && !jumping) ? 0 : timefalling + Time.deltaTime;
        if (grounded || (inadjustrange && !jumping))
        {
            sprinting = Input.GetKey(KeyCode.LeftShift);
            if (Input.GetKeyDown(KeyCode.Space) && !jumping)
            {
                Jump();
            }
            adjusty();
        }
        else 
        {
            Vector3 forces = new Vector3();
            forces += gravity * timefalling * timefalling * Vector3.down;
            rb.AddForce(forces);
        }

        //Move player relative to inputs
        Vector2 inputs = new Vector2(Input.GetAxis("Horizontal"), Input.GetAxis("Vertical"));
        
        //Speed up player if sprinting
        inputs = sprinting ? inputs * sprintmultiplier : inputs;
        inputs = incamera ? inputs * playercammult : inputs;
        Vector3 newvelocity = relativize(new Vector3(inputs.x * speed, rb.velocity.y, inputs.y * speed));
        rb.velocity = Vector3.Lerp(rb.velocity, newvelocity, !grounded || jumping ? .05f : .25f);
        
        //CameraRotation
        rotx += Input.GetAxis("Mouse X");
        roty = Mathf.Clamp(roty - Input.GetAxis("Mouse Y"), -70, 70);
        this.transform.eulerAngles = new Vector3(0f, rotx, 0f);
        cam.transform.localEulerAngles = new Vector3(roty, 0f, 0f);

        Debug.DrawRay(cam.transform.position, cam.transform.forward);
        if (Input.GetKeyDown(KeyCode.E))
            Interact();
        
        
        //Take out and put away camera, take photos, stuff.
        CameraFunctions();

        //Show the player walking and all that
        Vector2 stuff = new Vector2(rb.velocity.x, rb.velocity.z);
        anim.SetFloat("Speed",  Mathf.Abs(stuff.magnitude/(speed * sprintmultiplier)));

    }

    void CameraFunctions()
    {
        incamera = Input.GetMouseButton(1);
        cameraObject.SetActive(incamera);
        if (incamera && Input.GetMouseButtonDown(0))
        {
            ineditor = true;
            photocanvas.SetActive(true);
        }
}

    void Interact()
    {
        Debug.Log("Interacted!");
        RaycastHit hit;
        if (Physics.Raycast(new Ray(cam.transform.position, cam.transform.forward), out hit, InteractLength, NPCMASK))
        {
            string dialogue = hit.transform.gameObject.GetComponent<NPC>().Interact();
            dr.StartDialogue(dialogue);
        }
    }

    void Jump()
    {
        this.transform.position += Vector3.up * .25f ;
        rb.velocity = new Vector3(rb.velocity.x, 0f, rb.velocity.z);
        rb.AddForce(jumpstr * Vector3.up, ForceMode.Impulse);
        jumping = true;
    }
    void adjusty()
    {
        float difference = (this.transform.position.y - groundheight) - ground.point.y;
        rb.velocity = new Vector3(rb.velocity.x,
            Mathf.Lerp(rb.velocity.y, -(yadjstr * difference * Mathf.Abs(difference)), .5f),
            rb.velocity.z
            );
    }

    Vector3 relativize(Vector3 input)
    {
        return this.transform.TransformVector(input);
    }

    private void OnDrawGizmos()
    {
        var transform1 = this.transform;
        Gizmos.color = Color.white;
        Gizmos.DrawLine(transform1.position, transform1.position - transform1.up * groundheight*1.25f);
        Gizmos.color = Color.blue;
        Gizmos.DrawLine(transform1.position - transform1.up * groundheight, transform1.position - transform1.up * groundheight + transform1.right * .25f);
        Gizmos.color = Color.red;
        Gizmos.DrawLine(ground.point, ground.point + transform1.right * .25f);
    }
}
