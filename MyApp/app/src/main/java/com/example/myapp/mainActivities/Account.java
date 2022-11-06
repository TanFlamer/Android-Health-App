package com.example.myapp.mainActivities;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;

import com.example.myapp.R;

import java.util.HashMap;
import java.util.Map;

public class Account extends AppCompatActivity {

    Map<LinearLayout, Boolean> linearLayoutBooleanMap = new HashMap<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_account);

        Button buttonLogin = findViewById(R.id.buttonLogin);
        buttonLogin.setOnClickListener(view -> {
            startActivity(new Intent(getApplicationContext(), Music.class));
            overridePendingTransition(0, 0);
        });

        Button buttonDelete = findViewById(R.id.buttonDelete);
        buttonDelete.setOnClickListener(view -> new AlertDialog.Builder(this)
                .setTitle("Account Deletion")
                .setMessage("Are you sure you want to delete your account? There is no way to recover your account once deleted.")
                .setPositiveButton("Yes", null)
                .setNegativeButton("No", null)
                .create()
                .show());

        hideLayout(findViewById(R.id.layoutCreationVisible), findViewById(R.id.layoutCreationHidden));
        hideLayout(findViewById(R.id.layoutUsernameVisible), findViewById(R.id.layoutUsernameHidden));
        hideLayout(findViewById(R.id.layoutPasswordVisible), findViewById(R.id.layoutPasswordHidden));
        hideLayout(findViewById(R.id.layoutDeletionVisible), findViewById(R.id.layoutDeletionHidden));
    }

    public void hideLayout(LinearLayout layoutVisible, LinearLayout layoutHidden){
        layoutHidden.setVisibility(View.GONE);
        linearLayoutBooleanMap.put(layoutHidden, false);
        layoutVisible.setOnClickListener(view -> {
            linearLayoutBooleanMap.put(layoutHidden, Boolean.FALSE.equals(linearLayoutBooleanMap.get(layoutHidden)));
            layoutHidden.setVisibility(Boolean.TRUE.equals(linearLayoutBooleanMap.get(layoutHidden)) ? View.VISIBLE : View.GONE);
        });
    }
}