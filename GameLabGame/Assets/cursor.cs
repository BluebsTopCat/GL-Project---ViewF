using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class cursor : MonoBehaviour
{
   private bool locked = true;
   public void Update()
   {
      if (locked)
         Cursor.lockState = CursorLockMode.Locked;
      else
         Cursor.lockState = CursorLockMode.Confined;
      Cursor.visible = !locked;
   }

   public void enter()
   {
      locked = false;
   }

   public void exit()
   {
      locked = true;
   }
}
