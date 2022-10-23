package com.example.myappv2.customdialogs;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.DialogFragment;

import com.example.myappv2.R;

public class SleepDialogDeleteFragment extends DialogFragment {

    SleepDialogFragment sleepDialog;

    public void setSleepDialog(SleepDialogFragment sleepDialogFragment){
        sleepDialog = sleepDialogFragment;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        return inflater.inflate(R.layout.fragment_sleep_dialog_delete, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        Button delete = requireView().findViewById(R.id.sleepDelete3);
        delete.setOnClickListener(view1 -> {
            sleepDialog.dismiss();
        });

        Button close = requireView().findViewById(R.id.sleepClose);
        close.setOnClickListener(view1 -> {
            sleepDialog.dismiss();
        });
    }
}
