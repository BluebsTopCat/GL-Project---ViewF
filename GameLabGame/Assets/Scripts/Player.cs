using System;
using UnityEngine;
using Yarn.Unity;
using Random = UnityEngine.Random;

[RequireComponent(typeof(Rigidbody))]
[RequireComponent(typeof(Collider))]
[RequireComponent(typeof(Animator))]
public class Player : MonoBehaviour
{
    public enum Playerstate
    {
        Paused,
        Walking,
        Photoing,
        Talking
    };
    public Playerstate playerCurrently;
    [Header("RespawningPoint")] 
    public bool pause;
    public bool cansetrespawn = true;
    private Menu m;
    [HideInInspector]
    public Vector3 lastvalidpos;
    private Vector3 lastplacetouchingground;
    [Header("Camera")] 
    public bool ineditor;
    public bool incamera;
    public Animator caminator;
    public float playercammult;
    public GameObject disablecamera;
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

    [Header("Sound")]
    public AudioSource stepsfx;
    public AudioClip[] steps;

    private float startrot;

    private float crouching;

    private CapsuleCollider cld;
    // Start is called before the first frame update
    void Start()
    {
        startrot = this.transform.rotation.y;
        m = GameObject.FindObjectOfType<Menu>();
        cld = this.GetComponent<CapsuleCollider>();
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
        if (pause)
        {
            playerCurrently = Playerstate.Paused;
            rb.isKinematic = true;
            return;
        }
        
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

        crouching = Input.GetKey(KeyCode.LeftControl)? .5f : 0f;
        cld.height = 2 - crouching * 2;
        photocanvas.SetActive(false);
        Time.timeScale = 1;
        rb.isKinematic = false;
        inadjustrange = Physics.Raycast(transform.position + Vector3.up * crouching, Vector3.down,out ground, groundheight * 1.25f, groundlm);
        grounded = Vector3.Distance(ground.point, transform.position + Vector3.up * crouching) <= groundheight +.01f;
        
        if (Input.GetKeyDown(KeyCode.Escape))
        {
            m.pausegame();
        }

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
            caminator.SetBool("Up", false);
            return;
        }

        if (grounded || (inadjustrange && !jumping))
        {
            if (timefalling > .5f )  
            {
                stepsfx.pitch = Random.Range(.6f, .8f);
                stepsfx.PlayOneShot(steps[Random.Range(0, steps.Length - 1)]);;
            }
            
            if (lastplacetouchingground.y - transform.position.y >8)
            {
                cansetrespawn = false;
                m.outofbounds(Menu.oobtype.Fall);
            }

            timefalling = 0;
            
            sprinting = Input.GetKey(KeyCode.LeftShift);
            if (Input.GetKeyDown(KeyCode.Space) && !jumping)
            {
                Jump();
            }
            
            adjusty();
            lastplacetouchingground = this.transform.position;
        }
        else
        {
            timefalling += Time.deltaTime;
            Vector3 forces = new Vector3();
            forces += gravity * timefalling * timefalling * Vector3.down;
            rb.AddForce(forces);
        }

        if (grounded && timefalling < .15f && rb.velocity.y >= -2f && !pause && cansetrespawn)
        {
            updatelastpos();
        }

       
        //CameraRotation
        rotx += Input.GetAxis("Mouse X");
        roty = Mathf.Clamp(roty - Input.GetAxis("Mouse Y"), -70, 70);
        this.transform.eulerAngles = new Vector3(0f, rotx + 90, 0f);
        cam.transform.localEulerAngles = new Vector3(roty, 0f, 0f);

        //Move player relative to inputs
        Vector2 inputs = new Vector2(Input.GetAxis("Horizontal"), Input.GetAxis("Vertical"));
        
        inputs = sprinting ? inputs * sprintmultiplier : inputs;
        inputs = incamera ? inputs * playercammult : inputs;
        inputs *= (1 - crouching);
        
        Vector3 forwardVel = transform.forward * (inputs.y * speed);
        Vector3 rightVel = transform.right * (inputs.x * speed);
        Vector3 upVel = Vector3.up * rb.velocity.y;
        Vector3 newVel= forwardVel + upVel + rightVel;
        
        rb.velocity = Vector3.Lerp(rb.velocity, newVel, !grounded || jumping ? .05f : .25f);

        
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
        caminator.SetBool("Up", incamera);
        disablecamera.SetActive(incamera);
        if (incamera && Input.GetMouseButtonDown(0))
        {
            caminator.SetTrigger("Snap");
        }
    }

    public void snapped()
    {
        ineditor = true;
        photocanvas.SetActive(true);
    }

    void Interact()
    {
        Debug.Log("Interacted!");
        RaycastHit hit;
        if (Physics.Raycast(new Ray(cam.transform.position, cam.transform.forward), out hit, InteractLength, NPCMASK) &&
            hit.transform.gameObject.CompareTag("Finish"))
        {
            m.startend();
        }
        else if (Physics.Raycast(new Ray(cam.transform.position, cam.transform.forward), out hit, InteractLength, NPCMASK))
        {
            string dialogue = hit.transform.gameObject.GetComponent<NPC>().Interact();
            dr.StartDialogue(dialogue);
        }
    }

    void Jump()
    {
        stepsfx.pitch = Random.Range(.6f, .8f);
        stepsfx.PlayOneShot(steps[Random.Range(0, steps.Length - 1)]);
        this.transform.position += Vector3.up * .25f;
        rb.velocity = new Vector3(rb.velocity.x, 0f, rb.velocity.z);
        rb.AddForce(jumpstr * Vector3.up, ForceMode.Impulse);
        jumping = true;
    }
    void adjusty()
    {
        float difference = (this.transform.position.y + crouching - groundheight) - ground.point.y;
        rb.velocity = new Vector3(rb.velocity.x,
            Mathf.Lerp(rb.velocity.y, -(yadjstr * difference * Mathf.Abs(difference)), .5f),
            rb.velocity.z
            );
    }

    public void drown()
    {
        if (pause) return;
        pause = true;
        cansetrespawn = false;
        m.outofbounds(Menu.oobtype.Water);
    }

    public void step(AnimationEvent ae)
    {
        if (!inadjustrange || ae.animatorClipInfo.weight < .5f) return;
        stepsfx.pitch = Random.Range(1f, 1.4f);
        stepsfx.PlayOneShot(steps[Random.Range(0, steps.Length - 1)]);
    }
    

    public void respawn()
    {
        rb.velocity = Vector3.zero;
        this.transform.position = lastvalidpos;
    }
    void updatelastpos()
    {
        if (pause){ return;}
        
        Vector3[] raycasts = { new Vector3(0f,0f,0f), new Vector3(1f,0f,0f),new Vector3(-1f,0f,0f),new Vector3(0f,0f,1f),new Vector3(0f,0f,-1f)};
        bool fail = false;
        foreach (Vector3 offset in raycasts)
        {
            RaycastHit rch2;
            if (Physics.Raycast(transform.position + offset, -transform.up, out rch2, groundheight) && !rch2.collider.isTrigger && rch2.transform.gameObject.layer != 4)
            {
                Debug.DrawLine(transform.position + offset, transform.position + offset -transform.up * groundheight, Color.green);
            }
            else
            {
                Debug.DrawLine(transform.position + offset, transform.position + offset -transform.up * groundheight, Color.red);
                fail = true;
            }
        }

        if (fail) return; 
        lastvalidpos = transform.position;
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
