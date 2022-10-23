package com.example.myappv2.customdialogs;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.Spinner;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.DialogFragment;

import com.example.myappv2.R;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SleepDialogEditFragment extends DialogFragment {

    SleepDialogFragment sleepDialog;

    public void setSleepDialog(SleepDialogFragment sleepDialogFragment){
        sleepDialog = sleepDialogFragment;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        return inflater.inflate(R.layout.fragment_sleep_dialog_edit, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        List<Spinner> spinners = getSpinners(Arrays.asList(R.id.sleepHour, R.id.sleepMinute, R.id.sleepDay, R.id.wakeHour, R.id.wakeMinute, R.id.wakeDay));

        ArrayList<Integer> hours = new ArrayList<>();
        for(int i = 1; i <= 12; i++) hours.add(i);
        ArrayList<Integer> minutes = new ArrayList<>();
        for(int i = 1; i <= 60; i++) minutes.add(i);
        ArrayList<String> day = new ArrayList<>();
        day.add("AM"); day.add("PM");

        int count = 0;
        for(Spinner spinner : spinners){
            int remainder = count % 3;
            if(remainder == 0)
                spinner.setAdapter(new ArrayAdapter<>(getContext(), android.R.layout.simple_list_item_1, new ArrayList<>(hours)));
            else if(remainder == 1)
                spinner.setAdapter(new ArrayAdapter<>(getContext(), android.R.layout.simple_list_item_1, new ArrayList<>(minutes)));
            else
                spinner.setAdapter(new ArrayAdapter<>(getContext(), android.R.layout.simple_list_item_1, new ArrayList<>(day)));
            count++;
        }

        Button edit = requireView().findViewById(R.id.sleepEdit2);
        edit.setOnClickListener(view1 -> {
            Toast.makeText(getActivity(),"success", Toast.LENGTH_SHORT).show();
            sleepDialog.dismiss();
        });
    }

    public List<Spinner> getSpinners(List<Integer> spinnerID){
        ArrayList<Spinner> spinners = new ArrayList<>();
        for(Integer id : spinnerID) spinners.add((Spinner) requireView().findViewById(id));
        return spinners;
    }
}
